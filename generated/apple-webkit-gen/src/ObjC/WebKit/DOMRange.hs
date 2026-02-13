{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMRange@.
module ObjC.WebKit.DOMRange
  ( DOMRange
  , IsDOMRange(..)
  , setStart_offset
  , setEnd_offset
  , setStartBefore
  , setStartAfter
  , setEndBefore
  , setEndAfter
  , collapse
  , selectNode
  , selectNodeContents
  , compareBoundaryPoints_sourceRange
  , deleteContents
  , extractContents
  , cloneContents
  , insertNode
  , surroundContents
  , cloneRange
  , toString
  , detach
  , createContextualFragment
  , compareNode
  , intersectsNode
  , comparePoint_offset
  , isPointInRange_offset
  , setStart
  , setEnd
  , compareBoundaryPoints
  , startContainer
  , startOffset
  , endContainer
  , endOffset
  , collapsed
  , commonAncestorContainer
  , text
  , webArchive
  , markupString
  , cloneContentsSelector
  , cloneRangeSelector
  , collapseSelector
  , collapsedSelector
  , commonAncestorContainerSelector
  , compareBoundaryPointsSelector
  , compareBoundaryPoints_sourceRangeSelector
  , compareNodeSelector
  , comparePoint_offsetSelector
  , createContextualFragmentSelector
  , deleteContentsSelector
  , detachSelector
  , endContainerSelector
  , endOffsetSelector
  , extractContentsSelector
  , insertNodeSelector
  , intersectsNodeSelector
  , isPointInRange_offsetSelector
  , markupStringSelector
  , selectNodeContentsSelector
  , selectNodeSelector
  , setEndAfterSelector
  , setEndBeforeSelector
  , setEndSelector
  , setEnd_offsetSelector
  , setStartAfterSelector
  , setStartBeforeSelector
  , setStartSelector
  , setStart_offsetSelector
  , startContainerSelector
  , startOffsetSelector
  , surroundContentsSelector
  , textSelector
  , toStringSelector
  , webArchiveSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setStart:offset:@
setStart_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setStart_offset domRange refNode offset =
  sendMessage domRange setStart_offsetSelector (toDOMNode refNode) offset

-- | @- setEnd:offset:@
setEnd_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setEnd_offset domRange refNode offset =
  sendMessage domRange setEnd_offsetSelector (toDOMNode refNode) offset

-- | @- setStartBefore:@
setStartBefore :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setStartBefore domRange refNode =
  sendMessage domRange setStartBeforeSelector (toDOMNode refNode)

-- | @- setStartAfter:@
setStartAfter :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setStartAfter domRange refNode =
  sendMessage domRange setStartAfterSelector (toDOMNode refNode)

-- | @- setEndBefore:@
setEndBefore :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setEndBefore domRange refNode =
  sendMessage domRange setEndBeforeSelector (toDOMNode refNode)

-- | @- setEndAfter:@
setEndAfter :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setEndAfter domRange refNode =
  sendMessage domRange setEndAfterSelector (toDOMNode refNode)

-- | @- collapse:@
collapse :: IsDOMRange domRange => domRange -> Bool -> IO ()
collapse domRange toStart =
  sendMessage domRange collapseSelector toStart

-- | @- selectNode:@
selectNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
selectNode domRange refNode =
  sendMessage domRange selectNodeSelector (toDOMNode refNode)

-- | @- selectNodeContents:@
selectNodeContents :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
selectNodeContents domRange refNode =
  sendMessage domRange selectNodeContentsSelector (toDOMNode refNode)

-- | @- compareBoundaryPoints:sourceRange:@
compareBoundaryPoints_sourceRange :: (IsDOMRange domRange, IsDOMRange sourceRange) => domRange -> CUShort -> sourceRange -> IO CShort
compareBoundaryPoints_sourceRange domRange how sourceRange =
  sendMessage domRange compareBoundaryPoints_sourceRangeSelector how (toDOMRange sourceRange)

-- | @- deleteContents@
deleteContents :: IsDOMRange domRange => domRange -> IO ()
deleteContents domRange =
  sendMessage domRange deleteContentsSelector

-- | @- extractContents@
extractContents :: IsDOMRange domRange => domRange -> IO (Id DOMDocumentFragment)
extractContents domRange =
  sendMessage domRange extractContentsSelector

-- | @- cloneContents@
cloneContents :: IsDOMRange domRange => domRange -> IO (Id DOMDocumentFragment)
cloneContents domRange =
  sendMessage domRange cloneContentsSelector

-- | @- insertNode:@
insertNode :: (IsDOMRange domRange, IsDOMNode newNode) => domRange -> newNode -> IO ()
insertNode domRange newNode =
  sendMessage domRange insertNodeSelector (toDOMNode newNode)

-- | @- surroundContents:@
surroundContents :: (IsDOMRange domRange, IsDOMNode newParent) => domRange -> newParent -> IO ()
surroundContents domRange newParent =
  sendMessage domRange surroundContentsSelector (toDOMNode newParent)

-- | @- cloneRange@
cloneRange :: IsDOMRange domRange => domRange -> IO (Id DOMRange)
cloneRange domRange =
  sendMessage domRange cloneRangeSelector

-- | @- toString@
toString :: IsDOMRange domRange => domRange -> IO (Id NSString)
toString domRange =
  sendMessage domRange toStringSelector

-- | @- detach@
detach :: IsDOMRange domRange => domRange -> IO ()
detach domRange =
  sendMessage domRange detachSelector

-- | @- createContextualFragment:@
createContextualFragment :: (IsDOMRange domRange, IsNSString html) => domRange -> html -> IO (Id DOMDocumentFragment)
createContextualFragment domRange html =
  sendMessage domRange createContextualFragmentSelector (toNSString html)

-- | @- compareNode:@
compareNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO CShort
compareNode domRange refNode =
  sendMessage domRange compareNodeSelector (toDOMNode refNode)

-- | @- intersectsNode:@
intersectsNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO Bool
intersectsNode domRange refNode =
  sendMessage domRange intersectsNodeSelector (toDOMNode refNode)

-- | @- comparePoint:offset:@
comparePoint_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO CShort
comparePoint_offset domRange refNode offset =
  sendMessage domRange comparePoint_offsetSelector (toDOMNode refNode) offset

-- | @- isPointInRange:offset:@
isPointInRange_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO Bool
isPointInRange_offset domRange refNode offset =
  sendMessage domRange isPointInRange_offsetSelector (toDOMNode refNode) offset

-- | @- setStart::@
setStart :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setStart domRange refNode offset =
  sendMessage domRange setStartSelector (toDOMNode refNode) offset

-- | @- setEnd::@
setEnd :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setEnd domRange refNode offset =
  sendMessage domRange setEndSelector (toDOMNode refNode) offset

-- | @- compareBoundaryPoints::@
compareBoundaryPoints :: (IsDOMRange domRange, IsDOMRange sourceRange) => domRange -> CUShort -> sourceRange -> IO CShort
compareBoundaryPoints domRange how sourceRange =
  sendMessage domRange compareBoundaryPointsSelector how (toDOMRange sourceRange)

-- | @- startContainer@
startContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
startContainer domRange =
  sendMessage domRange startContainerSelector

-- | @- startOffset@
startOffset :: IsDOMRange domRange => domRange -> IO CInt
startOffset domRange =
  sendMessage domRange startOffsetSelector

-- | @- endContainer@
endContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
endContainer domRange =
  sendMessage domRange endContainerSelector

-- | @- endOffset@
endOffset :: IsDOMRange domRange => domRange -> IO CInt
endOffset domRange =
  sendMessage domRange endOffsetSelector

-- | @- collapsed@
collapsed :: IsDOMRange domRange => domRange -> IO Bool
collapsed domRange =
  sendMessage domRange collapsedSelector

-- | @- commonAncestorContainer@
commonAncestorContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
commonAncestorContainer domRange =
  sendMessage domRange commonAncestorContainerSelector

-- | @- text@
text :: IsDOMRange domRange => domRange -> IO (Id NSString)
text domRange =
  sendMessage domRange textSelector

-- | webArchive
--
-- A WebArchive representing the range.
--
-- ObjC selector: @- webArchive@
webArchive :: IsDOMRange domRange => domRange -> IO (Id WebArchive)
webArchive domRange =
  sendMessage domRange webArchiveSelector

-- | markupString
--
-- A markup string representing the range.
--
-- ObjC selector: @- markupString@
markupString :: IsDOMRange domRange => domRange -> IO (Id NSString)
markupString domRange =
  sendMessage domRange markupStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setStart:offset:@
setStart_offsetSelector :: Selector '[Id DOMNode, CInt] ()
setStart_offsetSelector = mkSelector "setStart:offset:"

-- | @Selector@ for @setEnd:offset:@
setEnd_offsetSelector :: Selector '[Id DOMNode, CInt] ()
setEnd_offsetSelector = mkSelector "setEnd:offset:"

-- | @Selector@ for @setStartBefore:@
setStartBeforeSelector :: Selector '[Id DOMNode] ()
setStartBeforeSelector = mkSelector "setStartBefore:"

-- | @Selector@ for @setStartAfter:@
setStartAfterSelector :: Selector '[Id DOMNode] ()
setStartAfterSelector = mkSelector "setStartAfter:"

-- | @Selector@ for @setEndBefore:@
setEndBeforeSelector :: Selector '[Id DOMNode] ()
setEndBeforeSelector = mkSelector "setEndBefore:"

-- | @Selector@ for @setEndAfter:@
setEndAfterSelector :: Selector '[Id DOMNode] ()
setEndAfterSelector = mkSelector "setEndAfter:"

-- | @Selector@ for @collapse:@
collapseSelector :: Selector '[Bool] ()
collapseSelector = mkSelector "collapse:"

-- | @Selector@ for @selectNode:@
selectNodeSelector :: Selector '[Id DOMNode] ()
selectNodeSelector = mkSelector "selectNode:"

-- | @Selector@ for @selectNodeContents:@
selectNodeContentsSelector :: Selector '[Id DOMNode] ()
selectNodeContentsSelector = mkSelector "selectNodeContents:"

-- | @Selector@ for @compareBoundaryPoints:sourceRange:@
compareBoundaryPoints_sourceRangeSelector :: Selector '[CUShort, Id DOMRange] CShort
compareBoundaryPoints_sourceRangeSelector = mkSelector "compareBoundaryPoints:sourceRange:"

-- | @Selector@ for @deleteContents@
deleteContentsSelector :: Selector '[] ()
deleteContentsSelector = mkSelector "deleteContents"

-- | @Selector@ for @extractContents@
extractContentsSelector :: Selector '[] (Id DOMDocumentFragment)
extractContentsSelector = mkSelector "extractContents"

-- | @Selector@ for @cloneContents@
cloneContentsSelector :: Selector '[] (Id DOMDocumentFragment)
cloneContentsSelector = mkSelector "cloneContents"

-- | @Selector@ for @insertNode:@
insertNodeSelector :: Selector '[Id DOMNode] ()
insertNodeSelector = mkSelector "insertNode:"

-- | @Selector@ for @surroundContents:@
surroundContentsSelector :: Selector '[Id DOMNode] ()
surroundContentsSelector = mkSelector "surroundContents:"

-- | @Selector@ for @cloneRange@
cloneRangeSelector :: Selector '[] (Id DOMRange)
cloneRangeSelector = mkSelector "cloneRange"

-- | @Selector@ for @toString@
toStringSelector :: Selector '[] (Id NSString)
toStringSelector = mkSelector "toString"

-- | @Selector@ for @detach@
detachSelector :: Selector '[] ()
detachSelector = mkSelector "detach"

-- | @Selector@ for @createContextualFragment:@
createContextualFragmentSelector :: Selector '[Id NSString] (Id DOMDocumentFragment)
createContextualFragmentSelector = mkSelector "createContextualFragment:"

-- | @Selector@ for @compareNode:@
compareNodeSelector :: Selector '[Id DOMNode] CShort
compareNodeSelector = mkSelector "compareNode:"

-- | @Selector@ for @intersectsNode:@
intersectsNodeSelector :: Selector '[Id DOMNode] Bool
intersectsNodeSelector = mkSelector "intersectsNode:"

-- | @Selector@ for @comparePoint:offset:@
comparePoint_offsetSelector :: Selector '[Id DOMNode, CInt] CShort
comparePoint_offsetSelector = mkSelector "comparePoint:offset:"

-- | @Selector@ for @isPointInRange:offset:@
isPointInRange_offsetSelector :: Selector '[Id DOMNode, CInt] Bool
isPointInRange_offsetSelector = mkSelector "isPointInRange:offset:"

-- | @Selector@ for @setStart::@
setStartSelector :: Selector '[Id DOMNode, CInt] ()
setStartSelector = mkSelector "setStart::"

-- | @Selector@ for @setEnd::@
setEndSelector :: Selector '[Id DOMNode, CInt] ()
setEndSelector = mkSelector "setEnd::"

-- | @Selector@ for @compareBoundaryPoints::@
compareBoundaryPointsSelector :: Selector '[CUShort, Id DOMRange] CShort
compareBoundaryPointsSelector = mkSelector "compareBoundaryPoints::"

-- | @Selector@ for @startContainer@
startContainerSelector :: Selector '[] (Id DOMNode)
startContainerSelector = mkSelector "startContainer"

-- | @Selector@ for @startOffset@
startOffsetSelector :: Selector '[] CInt
startOffsetSelector = mkSelector "startOffset"

-- | @Selector@ for @endContainer@
endContainerSelector :: Selector '[] (Id DOMNode)
endContainerSelector = mkSelector "endContainer"

-- | @Selector@ for @endOffset@
endOffsetSelector :: Selector '[] CInt
endOffsetSelector = mkSelector "endOffset"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector '[] Bool
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @commonAncestorContainer@
commonAncestorContainerSelector :: Selector '[] (Id DOMNode)
commonAncestorContainerSelector = mkSelector "commonAncestorContainer"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @webArchive@
webArchiveSelector :: Selector '[] (Id WebArchive)
webArchiveSelector = mkSelector "webArchive"

-- | @Selector@ for @markupString@
markupStringSelector :: Selector '[] (Id NSString)
markupStringSelector = mkSelector "markupString"

