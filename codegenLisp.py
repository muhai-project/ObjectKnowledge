import os
import sys
import yaml

#(defclass container (arrangeable)
#  ((contents :type list :initarg :contents :accessor contents :initform '()))
#  (:documentation "For objects that are containers (i.e. they have contents)."))
#(defclass kitchen-entity (entity)
#  ((persistent-id :type symbol :initarg :persistent-id :accessor persistent-id :initform nil)))
#(defmethod initialize-instance :after ((kitchen-entity kitchen-entity) &key)
#  (let ((persistent-id  (make-id (type-of kitchen-entity))))
#    (setf (persistent-id kitchen-entity) persistent-id)
#    (setf (id kitchen-entity) (make-id persistent-id))))
#(defmethod copy-object-content ((kitchen-state kitchen-state) (copy kitchen-state))
#  (setf (kitchen-time copy) (kitchen-time kitchen-state)))
#(defmethod copy-object-content ((container container) (copy container))
#  "Copying containers."
#  (setf (contents copy) (loop for item in (contents container)
#                              collect (copy-object item))))

def _inSuperclasses(classSpecs, className, propName):
    toCheck = set()
    toAdd = set(classSpecs[className].get("superclasses", []))
    while toAdd:
        cr = toAdd.pop()
        if (cr not in toCheck) and (cr in classSpecs):
            toCheck.add(cr)
            toAdd = toAdd.union(set(classSpecs[cr].get("superclasses", [])))
    present = False
    typeStrs = set()
    maximums = set()
    for e in toCheck:
        for f in (("dataproperties"), ("properties")):
            fs = classSpecs[e].get(f, {})
            if propName in fs:
                present = True
                if "range" in fs[propName]:
                    typeStrs.add(fs[propName]["range"])
                if "max" in fs[propName]:
                    maximums.add(fs[propName]["max"])
    typeStrs = sorted(list(typeStrs))
    maximums = sorted(list(maximums))
    if 1 < len(typeStrs):
        print("Warning: property %s has several types attached in the inheritance hierarchy: %s" % (propName, str(typeStrs)))
    if 1 < len(maximums):
        print("Warning: property %s has several maximums attached in the inheritance hierarchy: %s" % (propName, str(maximums)))
    if 0 == len(typeStrs):
        typeStrs = ['']
    if 0 == len(maximums):
        maximums = [None]
    return present, typeStrs[0], maximums[0]
    
def _getSpecification(classSpecs, className, propName, propSpec):
    writer = propSpec.get('accessor', propSpec.get('writer', propName))
    reader = propSpec.get('accessor', propSpec.get('reader', propName))
    maximum = propSpec.get('max', None)
    shared = propSpec.get('shared', False)
    initarg = propSpec.get('initarg', propName)
    initformYaml = propSpec.get('default', False)
    initform = "nil"
    if initformYaml is True:
        initform = 'T'
    elif initformYaml is not False:
        initform = str(initformYaml)
    typeStr = propSpec.get('range', '')
    declaredAbove, typeStrSuperclass, maximumSuperclass = _inSuperclasses(classSpecs, className, propName)
    if '' == typeStr:
        typeStr = typeStrSuperclass
    if maximum is None:
        maximum = maximumSuperclass
    return reader, writer, maximum, shared, initarg, initform, typeStr, declaredAbove
    
def _makeAccessor(reader, writer):
    accessW = ""
    accessR = ""
    if writer == reader:
        return (":accessor %s" % writer)
    else:
        if writer != '':
            accessW = (":writer %s" % writer)
        if reader != '':
            accessR = (":reader %s" % reader)
    if (0 < len(accessW)) and (0 < len(accessR)):
        return accessW + ' ' + accessR
    return accessW + accessR
    
def _makeTypeSpec(typeStr, maximum):
    if 1 == maximum:
        return typeStr
    if '' == typeStr:
        return 'list'
    return ("(list %s)" % typeStr)
    
def propertyLispDeclarationCode(classSpecs, className, propName, propSpec):
    reader, writer, maximum, shared, initarg, initform, typeStr, declaredAbove = _getSpecification(classSpecs, className, propName, propSpec)
    if declaredAbove:
        if "default" in propSpec:
            if 1 == maximum:
                return "  (%s :initform (make-instance '%s))\n" % (propName, initform)
            else:
                return "  (%s :initform (cons (make-instance '%s) nil))" % (propName, initform)
        return ""
    if "nil" == initform:
        return "  (%s %s\n      :initarg %s\n      %s\n      :initform nil)\n" % (propName, _makeTypeSpec(typeStr, maximum), initarg, _makeAccessor(reader, writer))
    elif "T" == initform:
        return "  (%s %s\n      :initarg %s\n      %s\n      :initform T)\n" % (propName, _makeTypeSpec(typeStr, maximum), initarg, _makeAccessor(reader, writer))
    if 1 != maximum:
        return "  (%s %s\n      :initarg %s\n      %s\n      :initform (cons (make-instance '%s) nil))\n" % (propName, _makeTypeSpec(typeStr, maximum), initarg, _makeAccessor(reader, writer), initform)
    return "  (%s %s\n      :initarg %s\n      %s\n      :initform (make-instance '%s))\n" % (propName, _makeTypeSpec(typeStr, maximum), initarg, _makeAccessor(reader, writer), initform)
    
def dataPropertyLispDeclarationCode(classSpecs, className, propName, propSpec):
    reader, writer, maximum, shared, initarg, initform, typeStr, declaredAbove = _getSpecification(classSpecs, className, propName, propSpec)
    if declaredAbove:
        if "default" in propSpec:
            return "  (%s :initform %s)\n" % (propName, initform)
        return ""
    return "  (%s %s\n      :initarg %s\n      %s\n      :initform %s)\n" % (propName, _makeTypeSpec(typeStr, maximum), initarg, _makeAccessor(reader, writer), initform)

def copyLispPropertyCode(classSpecs, className, propName, propSpec):
    reader, writer, maximum, shared, _, _, _, _ = _getSpecification(classSpecs, className, propName, propSpec)
    origValues = "(%s original)" % reader
    if not shared:
        origValues = "(copy-object %s)" % origValues
    if 1 != maximum:
        itemOrCopy = "item"
        if not shared:
            itemOrCopy = "(copy-object item)"
        origValues = "(loop for item in (%s original) collect %s)" % (reader, itemOrCopy)
    return "  (setf (%s copy) %s)\n" % (writer, origValues)

def copyLispDataPropertyCode(classSpecs, className, propName, propSpec):
    reader, writer, maximum, shared, _, _, _, _ = _getSpecification(classSpecs, className, propName, propSpec)
    origValues = "(%s original)" % reader
    if 1 != maximum:
        origValues = "(loop for item in %s collect item)" % origValues
    return "  (setf (%s copy) %s)\n" % (writer, origValues)

def generateClassCode(classSpecs, className):
    classSpec = classSpecs[className]
    docString = classSpec.get('documentation', '')
    lispSuperclassesList = classSpec.get('superclasses', [])
    lispSuperclasses = ' '.join(lispSuperclassesList)
    lispDataPropertiesDic = classSpec.get('dataproperties', {})
    lispPropertiesDic = classSpec.get('properties', {})
    duplicatePropertyNames = set(lispDataPropertiesDic.keys()).intersection(lispPropertiesDic.keys())
    if 0 < len(duplicatePropertyNames):
        print("WARNING: duplicate property names (same names used for both data and object properties): %s" % str(duplicatePropertyNames))
    lispProperties = ""
    for k in sorted(lispDataPropertiesDic.keys()):
        lispProperties = lispProperties + dataPropertyLispDeclarationCode(classSpecs, className, k, lispDataPropertiesDic[k])
    for k in sorted(lispPropertiesDic.keys()):
        lispProperties = lispProperties + propertyLispDeclarationCode(classSpecs, className, k, lispPropertiesDic[k])
    lispClassDef = "(defclass %s (%s)\n  (%s)\n  (:documentation \"%s\"))\n\n" % (className, lispSuperclasses, lispProperties[2:-1], docString)
    lispCopyMethodDef = ""
    if 0 < len(lispDataPropertiesDic) + len(lispPropertiesDic):
        propertyCopyCode = ""
        for k in sorted(lispDataPropertiesDic.keys()):
            propertyCopyCode = propertyCopyCode + copyLispDataPropertyCode(classSpecs, className, k, lispDataPropertiesDic[k])
        for k in sorted(lispPropertiesDic.keys()):
            propertyCopyCode = propertyCopyCode + copyLispPropertyCode(classSpecs, className, k, lispPropertiesDic[k])
        lispCopyMethodDef = "(defmethod copy-object-content (original %s) (copy %s)\n%s)\n\n" % (className, className, propertyCopyCode[:-1])
    return lispClassDef + lispCopyMethodDef
    
def main():
    if 3 > len(sys.argv):
        print("Need an input yaml file and an output file as command line arguments!\nExample usage:\ncodegenLisp onotology.yaml ontology.lisp")
        sys.exit(0)
    try:
        with open(sys.argv[1], "r") as infile:
            try:
                data = yaml.safe_load(infile)
            except yamlYAMLError as exc:
                print("Encountered error while parsing file:", exc)
                sys.exit(0)
    except Exception as exc:
        print(("Encountered error while accessing %s :" % sys.argv[1]), exc)
    classSpecs = data['classes']
    try:
        with open(sys.argv[2], "w") as outfile:
            outfile.write("(in-package :%s)\n\n" % data.get('lisp-package', 'muhai-cookingbot'))
            for className in sorted(classSpecs.keys()):
                outfile.write("%s\n" % generateClassCode(classSpecs, className))
    except Exception as exc:
        print(("Encountered error while writing to %s :" % sys.argv[2]), exc)

if "__main__" == __name__:
    main()
