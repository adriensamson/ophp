open Interpreter

let domImpl = Gdome.domImplementation ()

let domDocumentClass = new Object.phpClass "DOMDocument" false false false false None [] []

let _ = Extension.register
    "dom"
    []
    []
    [("DOMDocument", domDocumentClass)]
    []

