import yaml
import argparse

# (defclass container (arrangeable)
#  ((contents :type list :initarg :contents :accessor contents :initform '()))
#  (:documentation "For objects that are containers (i.e. they have contents)."))
# (defclass kitchen-entity (entity)
#  ((persistent-id :type symbol :initarg :persistent-id :accessor persistent-id :initform nil)))
# (defmethod initialize-instance :after ((kitchen-entity kitchen-entity) &key)
#  (let ((persistent-id  (make-id (type-of kitchen-entity))))
#    (setf (persistent-id kitchen-entity) persistent-id)
#    (setf (id kitchen-entity) (make-id persistent-id))))
# (defmethod copy-object-content ((kitchen-state kitchen-state) (copy kitchen-state))
#  (setf (kitchen-time copy) (kitchen-time kitchen-state)))
# (defmethod copy-object-content ((container container) (copy container))
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
        print(
            f"Warning: property {propName} has several types attached in the inheritance hierarchy: {typeStrs}"
        )
    if 1 < len(maximums):
        print(
            f"Warning: property {propName} has several maximums attached in the inheritance hierarchy: {maximums}"
        )
    if 0 == len(typeStrs):
        typeStrs = [""]
    if 0 == len(maximums):
        maximums = [None]
    return present, typeStrs[0], maximums[0]


def _getSpecification(classSpecs, className, propName, propSpec):
    writer = propSpec.get("accessor", propSpec.get("writer", propName))
    reader = propSpec.get("accessor", propSpec.get("reader", propName))
    maximum = propSpec.get("max", None)
    shared = propSpec.get("shared", False)
    initarg = propSpec.get("initarg", propName)
    initformYaml = propSpec.get("default", False)
    isPersistentID = propSpec.get("ispersistentid", False)
    isID = propSpec.get("isid", False)
    initform = "nil"
    if initformYaml is True:
        initform = "T"
    elif initformYaml is not False:
        initform = str(initformYaml)
    typeStr = propSpec.get("range", "")
    declaredAbove, typeStrSuperclass, maximumSuperclass = _inSuperclasses(
        classSpecs, className, propName
    )
    if "" == typeStr:
        typeStr = typeStrSuperclass
    if maximum is None:
        maximum = maximumSuperclass
    return (
        reader,
        writer,
        maximum,
        shared,
        initarg,
        initform,
        typeStr,
        declaredAbove,
        isPersistentID,
        isID,
    )


def _makeAccessor(reader, writer):
    accessW = ""
    accessR = ""
    if writer == reader:
        return f":accessor {writer}"
    else:
        if writer != "":
            accessW = f":writer {writer}"
        if reader != "":
            accessR = f":reader {reader}"
    if (0 < len(accessW)) and (0 < len(accessR)):
        return accessW + " " + accessR
    return accessW + accessR


def _makeTypeSpec(typeStr, maximum):
    if 1 == maximum:
        return typeStr
    return "list"
    # if maximum is None:
    #     return typeStr
    # return ("(list %s)" % typeStr)
    # return (or )


def propertyLispDeclarationCode(classSpecs, className, propName, propSpec):
    (
        reader,
        writer,
        maximum,
        shared,
        initarg,
        initform,
        typeStr,
        declaredAbove,
        _,
        _,
    ) = _getSpecification(classSpecs, className, propName, propSpec)
    if declaredAbove:
        if "default" in propSpec:
            if 1 == maximum:
                return f"  ({propName} :initform (make-instance '{initform}))\n"
            else:
                return (
                    f"  ({propName} :initform (cons (make-instance '{initform}) nil))"
                )
        return ""
    if "nil" == initform:
        return f"  ({propName} :type {_makeTypeSpec(typeStr, maximum)}\n      :initarg :{initarg}\n      {_makeAccessor(reader, writer)}\n      :initform nil)\n"
    elif "T" == initform:
        return f"  ({propName} :type {_makeTypeSpec(typeStr, maximum)}\n      :initarg :{initarg}\n      {_makeAccessor(reader, writer)}\n      :initform T)\n"
    if 1 != maximum:
        return f"  ({propName} :type {_makeTypeSpec(typeStr, maximum)}\n      :initarg :{initarg}\n      {_makeAccessor(reader, writer)}\n      :initform (cons (make-instance '{initform}) nil))\n"
    return f"  ({propName} :type {_makeTypeSpec(typeStr, maximum)}\n      :initarg :{initarg}\n      {_makeAccessor(reader, writer)}\n      :initform (make-instance '{initform}))\n"


def dataPropertyLispDeclarationCode(classSpecs, className, propName, propSpec):
    (
        reader,
        writer,
        maximum,
        shared,
        initarg,
        initform,
        typeStr,
        declaredAbove,
        _,
        _,
    ) = _getSpecification(classSpecs, className, propName, propSpec)
    if declaredAbove:
        if "default" in propSpec:
            return f"  ({propName} :initform {initform})\n"
        return ""
    return f"  ({propName} :type {_makeTypeSpec(typeStr, maximum)}\n      :initarg :{initarg}\n      {_makeAccessor(reader, writer)}\n      :initform {initform})\n"


def copyLispPropertyCode(classSpecs, className, propName, propSpec):
    (
        reader,
        writer,
        maximum,
        shared,
        _,
        _,
        _,
        _,
        isPersistentID,
        isID,
    ) = _getSpecification(classSpecs, className, propName, propSpec)
    if isPersistentID:
        return f"  (setf ({writer} copy) ({reader} original))\n"
    elif isID:
        return f"  (setf ({writer} copy) (make-id ({reader} original)))\n"
    origValues = f"({reader} original)"
    if not shared:
        origValues = f"(copy-object {origValues})"
    if 1 != maximum:
        itemOrCopy = "item"
        if not shared:
            itemOrCopy = "(copy-object item)"
        origValues = f"(loop for item in ({reader} original) collect {itemOrCopy})"
    return f"  (setf ({writer} copy) {origValues})\n"


def copyLispDataPropertyCode(classSpecs, className, propName, propSpec):
    reader, writer, maximum, shared, _, _, _, _, _, _ = _getSpecification(
        classSpecs, className, propName, propSpec
    )
    origValues = f"({reader} original)"
    if 1 != maximum:
        origValues = f"(loop for item in {origValues} collect item)"
    return f"  (setf ({writer} copy) {origValues})\n"


def generateClassCode(classSpecs, className):
    classSpec = classSpecs[className]
    docString = classSpec.get("documentation", "")
    lispSuperclassesList = classSpec.get("superclasses", [])
    lispSuperclasses = " ".join(lispSuperclassesList)
    lispDataPropertiesDic = classSpec.get("dataproperties", {})
    lispPropertiesDic = classSpec.get("properties", {})
    duplicatePropertyNames = set(lispDataPropertiesDic.keys()).intersection(
        lispPropertiesDic.keys()
    )
    if 0 < len(duplicatePropertyNames):
        print(
            f"WARNING: duplicate property names (same names used for both data and object properties): {str(duplicatePropertyNames)}"
        )
    lispProperties = ""
    for k in sorted(lispDataPropertiesDic.keys()):
        lispProperties = lispProperties + dataPropertyLispDeclarationCode(
            classSpecs, className, k, lispDataPropertiesDic[k]
        )
    for k in sorted(lispPropertiesDic.keys()):
        lispProperties = lispProperties + propertyLispDeclarationCode(
            classSpecs, className, k, lispPropertiesDic[k]
        )
    lispClassDef = f'(defclass {className} ({lispSuperclasses})\n  ({lispProperties[2:-1]})\n  (:documentation "{docString}"))\n\n'
    lispIniAfterMethodDef = ""
    innerString = ""
    for k in lispPropertiesDic.keys():
        if lispPropertiesDic[k].get("replacenullby", False):
            aux = _getSpecification(classSpecs, className, k, lispPropertiesDic[k])
            reader, writer, maximum = aux[0], aux[1], aux[2]
            if 1 == maximum:
                innerString = (
                    innerString
                    + f"  (when (null ({reader} orig)) (setf ({writer} orig) (make-instance '{lispPropertiesDic[k]['replacenullby']})))\n"
                )
        if 0 < len(lispPropertiesDic[k].get("musthave", [])):
            aux = _getSpecification(classSpecs, className, k, lispPropertiesDic[k])
            reader, writer, maximum = aux[0], aux[1], aux[2]
            if 1 != maximum:
                for e in lispPropertiesDic[k]["musthave"]:
                    innerString = (
                        innerString
                        + f"  (when (null (find '{e} ({reader} orig) :test (lambda (x y) (eq x (type-of y))))) (setf ({writer} orig) (cons (make-instance '{e}) ({reader} orig))))\n"
                    )
    persistentID = None
    regularID = None
    for k in lispPropertiesDic.keys():
        if lispPropertiesDic[k].get("ispersistentid", False):
            persistentID = k
        if lispPropertiesDic[k].get("isid", False):
            regularID = k
    if (persistentID is not None) and (regularID is not None):
        innerString = (
            innerString
            + f"  (let ((persistent-id (make-id (type-of orig))))\n    (setf ({ _getSpecification(classSpecs, className, persistentID, lispPropertiesDic[persistentID])[1]} orig) persistent-id)\n    (setf ({_getSpecification(classSpecs, className, regularID, lispPropertiesDic[regularID])[1]} orig) (make-id persistent-id)))\n"
        )
    elif persistentID is not None:
        innerString = (
            innerString
            + f"  (setf ({_getSpecification(classSpecs, className, persistentID, lispPropertiesDic[persistentID])[1]} orig) (make-id (type-of orig)))\n"
        )
    elif regularID is not None:
        innerString = (
            innerString
            + f"  (setf ({_getSpecification(classSpecs, className, regularID, lispPropertiesDic[regularID])[1]} orig) (make-id (type-of orig)))\n"
        )
    if "" != innerString:
        lispIniAfterMethodDef = f"(defmethod initialize-instance :after ((orig {className}) &key)\n{innerString[:-1]})\n\n"
    lispCopyMethodDef = ""
    if 0 < len(lispDataPropertiesDic) + len(lispPropertiesDic):
        propertyCopyCode = ""
        for k in sorted(lispDataPropertiesDic.keys()):
            propertyCopyCode = propertyCopyCode + copyLispDataPropertyCode(
                classSpecs, className, k, lispDataPropertiesDic[k]
            )
        for k in sorted(lispPropertiesDic.keys()):
            propertyCopyCode = propertyCopyCode + copyLispPropertyCode(
                classSpecs, className, k, lispPropertiesDic[k]
            )
        lispCopyMethodDef = f"(defmethod copy-object-content (original {className}) (copy {className})\n{propertyCopyCode[:-1]})\n\n"
    return lispClassDef + lispIniAfterMethodDef + lispCopyMethodDef


def main():
    argumentparser = argparse.ArgumentParser(
        prog="python codegenLisp.py ",
        description="This program generates generates a lisp ontoloty class hierarchy from the yaml specifications.",
        epilog="Example usage:\ncodegenLisp ontology.yaml ontology.lisp",
    )
    argumentparser.add_argument(
        "-i",
        "--infile",
        dest="infile",
        help="The input file, eg. `ontoloty.yaml`",
        type=str,
        required=True,
    )
    argumentparser.add_argument(
        "-o",
        "--outfile",
        dest="outfile",
        help="The output file eg. `ontology.lisp`",
        type=str,
        required=True,
    )
    args = argumentparser.parse_args()
    infile = args.infile
    outfile = args.outfile

    try:
        with open(infile, "r", encoding="utf-8") as infile:
            data = yaml.safe_load(infile)
    except yaml.YAMLError as exc:
        print("Encountered error while parsing file:", exc)
        return
    except FileNotFoundError as exc:
        print(f"Encountered error while accessing {infile} :", exc)

    classSpecs = data["classes"]

    try:
        with open(outfile, "w", encoding="utf-8") as f:
            f.write(f"(in-package :{data.get('lisp-package', 'muhai-cookingbot')})\n\n")
            # Disable sort for now to avoid subclasses being defined before superclasses
            # for className in sorted(classSpecs.keys()):
            for className in classSpecs.keys():
                f.write(f"{generateClassCode(classSpecs, className)}\n")
    except FileNotFoundError as exc:
        print(f"Encountered error while writing to {outfile} :", exc)


if "__main__" == __name__:
    main()
