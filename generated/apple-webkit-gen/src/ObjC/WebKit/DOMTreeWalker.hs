{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMTreeWalker@.
module ObjC.WebKit.DOMTreeWalker
  ( DOMTreeWalker
  , IsDOMTreeWalker(..)
  , parentNode
  , firstChild
  , lastChild
  , previousSibling
  , nextSibling
  , previousNode
  , nextNode
  , root
  , whatToShow
  , filter_
  , expandEntityReferences
  , currentNode
  , setCurrentNode
  , parentNodeSelector
  , firstChildSelector
  , lastChildSelector
  , previousSiblingSelector
  , nextSiblingSelector
  , previousNodeSelector
  , nextNodeSelector
  , rootSelector
  , whatToShowSelector
  , filterSelector
  , expandEntityReferencesSelector
  , currentNodeSelector
  , setCurrentNodeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- parentNode@
parentNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
parentNode domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "parentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firstChild@
firstChild :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
firstChild domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "firstChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastChild@
lastChild :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
lastChild domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "lastChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousSibling@
previousSibling :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
previousSibling domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "previousSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextSibling@
nextSibling :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
nextSibling domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "nextSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousNode@
previousNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
previousNode domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "previousNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextNode@
nextNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
nextNode domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "nextNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- root@
root :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
root domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "root") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- whatToShow@
whatToShow :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO CUInt
whatToShow domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "whatToShow") retCUInt []

-- | @- filter@
filter_ :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO RawId
filter_ domTreeWalker  =
    fmap (RawId . castPtr) $ sendMsg domTreeWalker (mkSelector "filter") (retPtr retVoid) []

-- | @- expandEntityReferences@
expandEntityReferences :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO Bool
expandEntityReferences domTreeWalker  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domTreeWalker (mkSelector "expandEntityReferences") retCULong []

-- | @- currentNode@
currentNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
currentNode domTreeWalker  =
    sendMsg domTreeWalker (mkSelector "currentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentNode:@
setCurrentNode :: (IsDOMTreeWalker domTreeWalker, IsDOMNode value) => domTreeWalker -> value -> IO ()
setCurrentNode domTreeWalker  value =
  withObjCPtr value $ \raw_value ->
      sendMsg domTreeWalker (mkSelector "setCurrentNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector
parentNodeSelector = mkSelector "parentNode"

-- | @Selector@ for @firstChild@
firstChildSelector :: Selector
firstChildSelector = mkSelector "firstChild"

-- | @Selector@ for @lastChild@
lastChildSelector :: Selector
lastChildSelector = mkSelector "lastChild"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @root@
rootSelector :: Selector
rootSelector = mkSelector "root"

-- | @Selector@ for @whatToShow@
whatToShowSelector :: Selector
whatToShowSelector = mkSelector "whatToShow"

-- | @Selector@ for @filter@
filterSelector :: Selector
filterSelector = mkSelector "filter"

-- | @Selector@ for @expandEntityReferences@
expandEntityReferencesSelector :: Selector
expandEntityReferencesSelector = mkSelector "expandEntityReferences"

-- | @Selector@ for @currentNode@
currentNodeSelector :: Selector
currentNodeSelector = mkSelector "currentNode"

-- | @Selector@ for @setCurrentNode:@
setCurrentNodeSelector :: Selector
setCurrentNodeSelector = mkSelector "setCurrentNode:"

