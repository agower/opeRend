#include <Rcpp.h>
using namespace Rcpp;

// C++ code for conversion of Operend list objects to R S4 objects
// Adam Gower

// Convenience function to return the class of an object
// as it would be reported by the R class() function
String klass (RObject x) {
  String result;
  if (x.isObject()) {
    // An S4 object will have a 'class' attribute
    result = as<String>(x.attr("class"));
  } else {
    // Other than S4 objects, this code will only expect atomic objects or lists
    result = Rf_type2str(TYPEOF(x));
    if (result == "double") result = "numeric";
  }
  return result;
}

// [[Rcpp::export]]
S4 operendListToS4 (List x, S4 object)
{
  /*

  This is a function to convert a list object (parsed from JSON returned by the
  Operend Core server) to the corresponding R S4 object.  It is called
  initially from operendPostprocess() and may be called recursively as needed.

  INPUT
    x       A List object as returned by fromJSON() in operendApiCall()
    object  An S4 object of the target class;
            if its class is "operendEntity", it also contains a slot named .Data
            with a list that is assumed to contain all possible variables in the
            EntityClass, in the appropriate classes (and, for Code/factor
            variables, with the appropriate levels)

  OUTPUT
    An S4 object of the same class as argument 'object', containing all
    elements of 'x', converted to slots accordingly; if the class of 'object' is
    "operendEntity", slot .Data includes only those Entity variables
    represented in 'x'

  */

  // Declare variables, in order of introduction
  R_xlen_t i, j;          // iterators
  CharacterVector xNames = x.names();
  List Data;           // contents of slot .Data
  CharacterVector toClasses, toClassesNames;
  std::string name;
  String fromClass, toClass;
  bool isDataElement;  // does an element of 'x' contain an element of .Data?
  RObject value;
  // Variables used in conversion to SimpleList objects
  List listData;       // contents of SimpleList slot 'listData'
  String elementType;  // contents of SimpleList slot 'elementType'

  // R functions needed for conversion
  Function asFn("as", "methods");
  Function extendsFn("extends", "methods");
  Function factorFn("factor", "base");
  Function unlistFn("unlist", "base");
  
  // Declare variables using argument 'object'
  String Class = as<String>(object.attr("class"));
  bool isEntity = (Class == "operendEntity");
  RObject classDef = R_getClassDef(Class.get_cstring());
  List slots = classDef.slot("slots");
  CharacterVector slotNames = slots.names();

  if (isEntity) {
    // Extract the .Data slot of the object to a named list
    Data = object.slot(".Data");
    Data.names() = object.attr("names");
    // Store Data.names() in a variable to avoid trouble
    // with passing names-attribute proxies to match()
    CharacterVector dataNames = Data.names();
    IntegerVector matches = match(dataNames, xNames);
    // Remove any elements of .Data that are not represented in 'x'
    // Note: removal proceeds in reverse order so index 'i' remains valid
    //       even after an element is removed from the list
    for (i = Data.size()-1; i >= 0; i--) {
      if (IntegerVector::is_na(matches[i])) {
        Data.erase(i);
      }
    }

    // Initialize vector 'toClasses' to hold classes to which each value will be
    // converted, as well as a vector to hold its names
    toClasses = CharacterVector(slots.size()-1 + Data.size());
    toClassesNames = CharacterVector(toClasses.size());
    // Add the class of each slot to the toClasses vector,
    // skipping over the slot ".Data" that holds the Entity variables
    for (i = 0, j = 0; i < slots.size(); i++) {
      if (slotNames[i] != ".Data") {
        toClasses[j] = as<String>(slots[i]);
        toClassesNames[j] = slotNames[i];
        j++;
      }
    }
    // Add class of each Entity variable to vector 'toClasses'
    for (i = 0; i < Data.size(); i++, j++) {
      toClasses[j] = klass(Data[i]);
      toClassesNames[j] = as<CharacterVector>(Data.names())[i];
    }
  } else {
    // Copy contents and names of list 'slots' into vectors
    toClasses = CharacterVector(slots.size());
    for (i = 0; i < slots.size(); i++) {
      toClasses[i] = as<String>(slots[i]);
    }
    toClassesNames = slotNames;
  }
  // Name vector 'toClasses'
  toClasses.names() = toClassesNames;

  // Iterate over each element of 'x', converting it to the proper class
  for (i = 0; i < x.size(); i++) {
    name = as<std::string>(xNames[i]);
    fromClass = klass(x[i]);
    toClass = as<String>(toClasses[name]);
    isDataElement = (isEntity && name[0] != '_');
    if (fromClass == toClass) {
      value = x[i];
    } else {
      if (isDataElement) {
        value = Data[name];
      } else {
        value = object.slot(name);
      }
      if (as<bool>(extendsFn(klass(value), "SimpleList"))) {
        // Objects that are an extension of "SimpleList" are handled specially
        listData = as<List>(x[i]);
        elementType = as<String>(value.slot("elementType"));
        if (elementType == "integer" || elementType == "character") {
          // This case is for objects of the following classes:
          // operendPermissions, operendWorkFileIdList, operendJobRunParameters
          for (j = 0; j < listData.size(); j++) {
            listData[j] = Rf_coerceVector(
              listData[j], Rf_str2type(elementType.get_cstring())
            );
          }
        } else {
          // This case is for all other SimpleList objects;
          // operendListToS4 is called for each sublist to create a sub-object
          for (j = 0; j < listData.size(); j++) {
            S4 subObject(elementType);
            listData[j] = operendListToS4(listData[j], subObject);
          }
        }
        value.slot("listData") = listData;
      } else if (fromClass == "list" && toClass == "character") {
        // This case is for the 'codes' element of an EntityClass variable
        value = as<CharacterVector>(unlistFn(x[i]));
      } else if (fromClass == "character" && toClass == "factor") {
        // This case is for a code-type Entity variable
        value = factorFn(x[i], value.attr("levels"));
      } else {
        // This final case uses as() to look up an appropriate coercion method
        value = asFn(x[i], toClass);
      }
    }
    // Store the converted value
    if (isDataElement) {
      Data[name] = value;
    } else {
      object.slot(name) = value;
    }
  }
  // If this is an Entity, copy list 'Data' back to slot '.Data'
  if (isEntity) {
    object.slot(".Data") = Data;
  }

  // Return the object containing the converted values
  return object;
}

