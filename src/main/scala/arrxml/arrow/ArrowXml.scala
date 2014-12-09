package arrxml.arrow

import arrxml.{ Node, TextNode, Comment, Element }

import annotation.implicitNotFound

@implicitNotFound(msg = "No instance in scope for ArrowXml[${=>>}].")
trait ArrowXml[=>>[-_, +_]] extends ArrowList[=>>] with ArrowTree[=>>] {

   // These types are not yet implemented.
   type QName = String
   type Blob = String
   type DTDElem = String
   type Attributes = String
   type Nodes = String

   /**
    * test for text nodes
    */
   def isText : Node =>> Node = isA { (n : Node) ⇒
      n match {
         case n : TextNode ⇒ true
         case _            ⇒ false
      }
   }

   def isBlob : Node =>> Node

   /**
    * test for char reference, used during parsing
    */
   def isCharRef : Node =>> Node

   /**
    * test for entity reference, used during parsing
    */
   def isEntityRef : Node =>> Node

   /**
    * test for comment
    */
   def isCmt : Node =>> Node = isA { (n : Node) ⇒
      n match {
         case n : Comment ⇒ true
         case _           ⇒ false
      }
   }

   /**
    * test for CDATA section, used during parsing
    */
   def isCdata : Node =>> Node

   /**
    * test for processing instruction
    */
   def isPi : Node =>> Node

   /**
    * test for processing instruction \<?xml ...\>
    */
   def isXmlPi : Node =>> Node

   /**
    * test for element
    */
   def isElem : Node =>> Node = isA { (n : Node) ⇒
      n match {
         case n : Element ⇒ true
         case _           ⇒ false
      }
   }

   /**
    * test for DTD part, used during parsing
    */
   def isDTD : Node =>> Node

   /**
    * test for attribute tree
    */
   def isAttr : Node =>> Node

   /**
    * test for error message
    */
   def isError : Node =>> Node

   /**
    * test for root node (element with name \"\/\")
    */
   def isRoot : Node =>> Node

   /**
    * test for text nodes with text, for which a predicate holds
    *
    * example: @hasText (all (\`elem\` \" \\t\\n\"))@ check for text nodes with only whitespace content
    */
   def hasText(predicate : String ⇒ Boolean) : Node =>> Node =
      guards(>>>(>>>(isText, getText), isA(predicate)))(self)

   /**
    * test for text nodes with only white space
    *
    * implemented with 'hasTest'
    */

   def isWhiteSpace : Node =>> Node

   /**
    * |
    * test whether a node (element, attribute, pi) has a name with a special property
    */
   def hasNameWith(predicate : QName ⇒ Boolean) : Node =>> Node

   /**
    * |
    * test whether a node (element, attribute, pi) has a specific qualified name
    * useful only after namespace propagation
    */
   def hasQName(name : QName) : Node =>> Node

   /**
    * |
    * test whether a node has a specific name (prefix:localPart ore localPart),
    * generally useful, even without namespace handling
    */
   def hasName(name : String) : Node =>> Node

   /**
    * |
    * test whether a node has a specific name as local part,
    * useful only after namespace propagation
    */
   def hasLocalPart(lp : String) : Node =>> Node

   /**
    * |
    * test whether a node has a specific name prefix,
    * useful only after namespace propagation
    */
   def hasNamePrefix(pre : String) : Node =>> Node

   /**
    * |
    * test whether a node has a specific namespace URI
    * useful only after namespace propagation
    */
   def hasNamespaceUri(nameSpace : String) : Node =>> Node

   /**
    * |
    * test whether an element node has an attribute node with a specific name
    */
   def hasAttr(attr : String) : Node =>> Node

   /**
    * |
    * test whether an element node has an attribute node with a specific qualified name
    */
   def hasQAttr(attr : QName) : Node =>> Node

   /**
    * |
    * test whether an element node has an attribute with a specific value
    */
   def hasAttrValue(attrName : String)(predicate : String ⇒ Boolean) : Node =>> Node

   /**
    * |
    * test whether an element node has an attribute with a qualified name and a specific value
    */
   def hasQAttrValue(attrName : QName)(predicate : String ⇒ Boolean) : Node =>> Node

   /**
    * text node construction arrow
    */
   def mkText : String =>> Node

   /**
    * blob node construction arrow
    */
   def mkBlob : Blob =>> Node

   /**
    * char reference construction arrow, useful for document output
    */
   def mkCharRef : Int =>> Node

   /**
    * entity reference construction arrow, useful for document output
    */
   def mkEntityRef : String =>> Node

   /**
    * comment node construction, useful for document output
    */
   def mkCmt : String =>> Node

   /**
    * CDATA construction, useful for document output
    */
   def mkCdata : String =>> Node

   /**
    * error node construction, useful only internally
    */
   def mkError(i : Int) : String =>> Node

   /**
    * element construction:
    * the attributes and the content of the element are computed by applying arrows
    * to the input
    */
   def mkElement[N](name : QName)(a1 : N =>> Node)(a2 : N =>> Node) : N =>> Node

   /**
    * attribute node construction:
    * the attribute value is computed by applying an arrow to the input
    */
   def mkAttr[N](name : QName)(a1 : N =>> Node) : N =>> Node

   /**
    * processing instruction construction:
    * the content of the processing instruction is computed by applying an arrow to the input
    */
   def mkPi[N](name : QName)(a1 : N =>> Node) : N =>> Node

   /**
    * convenient arrow for element construction, more comfortable variant of 'mkElement'
    *
    * example for simplifying 'mkElement' :
    *
    * > mkElement qn (a1 <+> ... <+> ai) (c1 <+> ... <+> cj)
    *
    * equals
    *
    * > mkqelem qn [a1,...,ai] [c1,...,cj]
    */
   def mkqelem[N](name : QName)(a1 : List[N =>> Node])(a2 : List[N =>> Node]) : N =>> Node

   /**
    * convenient arrow for element construction with strings instead of qualified names as element names, see also 'mkElement' and 'mkelem'
    */
   def mkelem[N](name : String)(a1 : List[N =>> Node])(a2 : List[N =>> Node]) : N =>> Node

   /**
    * convenient arrow for element constrution with attributes but without content, simple variant of 'mkelem' and 'mkElement'
    */
   def aelem[N](name : String)(a1 : List[N =>> Node]) : N =>> Node

   /**
    * convenient arrow for simple element constrution without attributes, simple variant of 'mkelem' and 'mkElement'
    */
   def selem[N](name : String)(a1 : List[N =>> Node]) : N =>> Node

   /**
    * convenient arrow for constrution of empty elements without attributes, simple variant of 'mkelem' and 'mkElement'
    */
   def eelem[N](name : String) : N =>> Node

   /**
    * construction of an element node with name \"\/\" for document roots
    */
   def root[N](a1 : List[N =>> Node])(a2 : List[N =>> Node]) : N =>> Node

   /**
    * alias for 'mkAttr'
    */
   def qattr[N](name : QName)(a1 : N =>> Node) : N =>> Node

   /**
    * convenient arrow for attribute constrution, simple variant of 'mkAttr'
    */
   def attr[N](name : String)(a1 : N =>> Node) : N =>> Node

   /**
    * constant arrow for text nodes
    */
   def txt[N](content : String) : N =>> Node

   /**
    * constant arrow for blob nodes
    */
   def blb[N](content : Blob) : N =>> Node

   /**
    * constant arrow for char reference nodes
    */
   def charRef[N](i : Int) : N =>> Node

   /**
    * constant arrow for entity reference nodes
    */
   def entityRef[N](ref : String) : N =>> Node

   /**
    * constant arrow for comment
    */
   def cmt[N](content : String) : N =>> Node

   /**
    * constant arrow for warning
    */
   def warn[N](msg : String) : N =>> Node

   /**
    * constant arrow for errors
    */
   def err[N](msg : String) : N =>> Node

   /**
    * constant arrow for fatal errors
    */
   def fatal[N](msg : String) : N =>> Node

   /**
    * constant arrow for simple processing instructions, see 'mkPi'
    */
   def spi[N](a : String, b : String) : N =>> Node

   /**
    * constant arrow for attribute nodes, attribute name is a qualified name and value is a text,
    * see also 'mkAttr', 'qattr', 'attr'
    */
   def sqattr[N](n : QName, v : String) : N =>> Node

   /**
    * constant arrow for attribute nodes, attribute name and value are
    * given by parameters, see 'mkAttr'
    */
   def sattr[N](n : String, v : String) : N =>> Node

   /**
    * select the text of a text node
    */
   def getText : Node =>> String

   /**
    * select the value of a char reference
    */
   def getCharRef : Node =>> Int

   /**
    * select the name of a entity reference node
    */
   def getEntityRef : Node =>> String

   /**
    * select the comment of a comment node
    */
   def getCmt : Node =>> String

   /**
    * select the content of a CDATA node
    */
   def getCdata : Node =>> String

   /**
    * select the name of a processing instruction
    */
   def getPiName : Node =>> QName

   /**
    * select the content of a processing instruction
    */
   def getPiContent : Node =>> Node

   /**
    * select the name of an element node
    */
   def getElemName : Node =>> QName

   /**
    * select the attribute list of an element node
    */
   def getAttrl : Node =>> Node

   /**
    * select the DTD type of a DTD node
    */
   def getDTDPart : Node =>> DTDElem

   /**
    * select the DTD attributes of a DTD node
    */
   def getDTDAttrl : Node =>> Attributes

   /**
    * select the name of an attribute
    */
   def getAttrName : Node =>> QName

   /**
    * select the error level (c_warn, c_err, c_fatal) from an error node
    */
   def getErrorLevel : Node =>> Int

   /**
    * select the error message from an error node
    */
   def getErrorMsg : Node =>> String

   /**
    * select the qualified name from an element, attribute or pi
    */
   def getQName : Node =>> QName

   /**
    * select the prefix:localPart or localPart from an element, attribute or pi
    */
   def getName : Node =>> String

   /**
    * select the univeral name ({namespace URI} ++ localPart)
    */
   def getUniversalName : Node =>> String

   /**
    * select the univeral name (namespace URI ++ localPart)
    */
   def getUniversalUri : Node =>> String

   /**
    * select the local part
    */
   def getLocalPart : Node =>> String

   /**
    * select the name prefix
    */
   def getNamePrefix : Node =>> String

   /**
    * select the namespace URI
    */
   def getNamespaceUri : Node =>> String

   /**
    * select the value of an attribute of an element node,
    * always succeeds with empty string as default value \"\"
    */
   def getAttrValue : String ⇒ (Node =>> String)

   /**
    * like 'getAttrValue', but fails if the attribute does not exist
    */
   def getAttrValue0 : String ⇒ (Node =>> String)

   /**
    * like 'getAttrValue', but select the value of an attribute given by a qualified name,
    * always succeeds with empty string as default value \"\"
    */
   def getQAttrValue : QName ⇒ (Node =>> String)

   /**
    * like 'getQAttrValue', but fails if attribute does not exist
    */
   def getQAttrValue0 : QName ⇒ (Node =>> String)

   /**
    * edit the string of a text node
    */
   def changeText : (String ⇒ String) ⇒ (Node =>> Node)

   /**
    * edit the blob of a blob node
    */
   def changeBlob : (Blob ⇒ Blob) ⇒ (Node =>> Node)

   /**
    * edit the comment string of a comment node
    */
   def changeCmt : (String ⇒ String) ⇒ (Node =>> Node)

   /**
    * edit an element-, attribute- or pi- name
    */
   def changeQName : (QName ⇒ QName) ⇒ (Node =>> Node)

   /**
    * edit an element name
    */
   def changeElemName : (QName ⇒ QName) ⇒ (Node =>> Node)

   /**
    * edit an attribute name
    */
   def changeAttrName : (QName ⇒ QName) ⇒ (Node =>> Node)

   /**
    * edit a pi name
    */
   def changePiName : (QName ⇒ QName) ⇒ (Node =>> Node)

   /**
    * edit an attribute value
    */
   def changeAttrValue : (String ⇒ String) ⇒ (Node =>> Node)

   /**
    *
    * edit an attribute list of an element node
    */
   def changeAttrl : (Nodes ⇒ Nodes ⇒ Nodes) ⇒ (Node =>> Node) ⇒ (Node =>> Node)

   /**
    * replace an element, attribute or pi name
    */
   def setQName : QName ⇒ (Node =>> Node)

   /**
    * replace an element name
    */
   def setElemName : QName ⇒ (Node =>> Node)

   /**
    * replace an attribute name
    */
   def setAttrName : QName ⇒ (Node =>> Node)

   /**
    * replace an element name
    */
   def setPiName : QName ⇒ (Node =>> Node)

   /**
    * replace an atribute list of an element node
    */
   def setAttrl : (Node =>> Node) ⇒ (Node =>> Node)

   /**
    * add a list of attributes to an element
    */
   def addAttrl : (Node =>> Node) ⇒ (Node =>> Node)

   /**
    * add (or replace) an attribute
    */
   def addAttr : String ⇒ String ⇒ (Node =>> Node)

   /**
    * remove an attribute
    */
   def removeAttr : String ⇒ (Node =>> Node)

   /**
    * remove an attribute with a qualified name
    */
   def removeQAttr : QName ⇒ (Node =>> Node)

   /**
    * process the attributes of an element node with an arrow
    */
   def processAttrl : (Node =>> Node) ⇒ (Node =>> Node)

   /**
    * process a whole tree inclusive attribute list of element nodes
    * see also: 'Control.Arrow.ArrowTree.processTopDown'
    */
   def processTopDownWithAttrl : (Node =>> Node) ⇒ (Node =>> Node)

   /**
    * convenient op for adding attributes or children to a node
    *
    * usage: @ tf += cf @
    *
    * the @tf@ arrow computes an element node, and all trees computed by @cf@ are
    * added to this node, if a tree is an attribute, it is inserted in the attribute list
    * else it is appended to the content list.
    *
    * attention: do not build long content list this way because '+=' is implemented by ++
    *
    * examples:
    *
    * > eelem "a"
    * >   += sattr "href" "page.html"
    * >   += sattr "name" "here"
    * >   += txt "look here"
    *
    * is the same as
    *
    * > mkelem [ sattr "href" "page.html"
    * >        , sattr "name" "here"
    * >        ]
    * >        [ txt "look here" ]
    *
    * and results in the XML fragment: \<a href=\"page.html\" name=\"here\"\>look here\<\/a\>
    *
    * advantage of the '+=' operator is, that attributes and content can be added
    * any time step by step.
    * if @tf@ computes a whole list of trees, e.g. a list of \"td\" or \"tr\" elements,
    * the attributes or content is added to all trees. useful for adding \"class\" or \"style\" attributes
    * to table elements.
    */

   def +=[B] : (B =>> Node) ⇒ (B =>> Node) ⇒ (B =>> Node)

   /**
    * apply an arrow to the input and convert the resulting XML trees into a string representation
    *
    */
   def xshow[N] : (N =>> Node) ⇒ (N =>> String)

   /**
    * apply an arrow to the input and convert the resulting XML trees into a string representation
    *
    */
   def xshowBlob[N] : (N =>> Node) ⇒ (N =>> Blob)
}

object ArrowXml {
   @inline def apply[F[-_, +_]](implicit ev : ArrowXml[F]) : ArrowXml[F] = ev
}
