{-# LANGUAGE DataKinds #-}
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
  , currentNodeSelector
  , expandEntityReferencesSelector
  , filterSelector
  , firstChildSelector
  , lastChildSelector
  , nextNodeSelector
  , nextSiblingSelector
  , parentNodeSelector
  , previousNodeSelector
  , previousSiblingSelector
  , rootSelector
  , setCurrentNodeSelector
  , whatToShowSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- parentNode@
parentNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
parentNode domTreeWalker =
  sendMessage domTreeWalker parentNodeSelector

-- | @- firstChild@
firstChild :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
firstChild domTreeWalker =
  sendMessage domTreeWalker firstChildSelector

-- | @- lastChild@
lastChild :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
lastChild domTreeWalker =
  sendMessage domTreeWalker lastChildSelector

-- | @- previousSibling@
previousSibling :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
previousSibling domTreeWalker =
  sendMessage domTreeWalker previousSiblingSelector

-- | @- nextSibling@
nextSibling :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
nextSibling domTreeWalker =
  sendMessage domTreeWalker nextSiblingSelector

-- | @- previousNode@
previousNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
previousNode domTreeWalker =
  sendMessage domTreeWalker previousNodeSelector

-- | @- nextNode@
nextNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
nextNode domTreeWalker =
  sendMessage domTreeWalker nextNodeSelector

-- | @- root@
root :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
root domTreeWalker =
  sendMessage domTreeWalker rootSelector

-- | @- whatToShow@
whatToShow :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO CUInt
whatToShow domTreeWalker =
  sendMessage domTreeWalker whatToShowSelector

-- | @- filter@
filter_ :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO RawId
filter_ domTreeWalker =
  sendMessage domTreeWalker filterSelector

-- | @- expandEntityReferences@
expandEntityReferences :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO Bool
expandEntityReferences domTreeWalker =
  sendMessage domTreeWalker expandEntityReferencesSelector

-- | @- currentNode@
currentNode :: IsDOMTreeWalker domTreeWalker => domTreeWalker -> IO (Id DOMNode)
currentNode domTreeWalker =
  sendMessage domTreeWalker currentNodeSelector

-- | @- setCurrentNode:@
setCurrentNode :: (IsDOMTreeWalker domTreeWalker, IsDOMNode value) => domTreeWalker -> value -> IO ()
setCurrentNode domTreeWalker value =
  sendMessage domTreeWalker setCurrentNodeSelector (toDOMNode value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector '[] (Id DOMNode)
parentNodeSelector = mkSelector "parentNode"

-- | @Selector@ for @firstChild@
firstChildSelector :: Selector '[] (Id DOMNode)
firstChildSelector = mkSelector "firstChild"

-- | @Selector@ for @lastChild@
lastChildSelector :: Selector '[] (Id DOMNode)
lastChildSelector = mkSelector "lastChild"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector '[] (Id DOMNode)
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector '[] (Id DOMNode)
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector '[] (Id DOMNode)
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector '[] (Id DOMNode)
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @root@
rootSelector :: Selector '[] (Id DOMNode)
rootSelector = mkSelector "root"

-- | @Selector@ for @whatToShow@
whatToShowSelector :: Selector '[] CUInt
whatToShowSelector = mkSelector "whatToShow"

-- | @Selector@ for @filter@
filterSelector :: Selector '[] RawId
filterSelector = mkSelector "filter"

-- | @Selector@ for @expandEntityReferences@
expandEntityReferencesSelector :: Selector '[] Bool
expandEntityReferencesSelector = mkSelector "expandEntityReferences"

-- | @Selector@ for @currentNode@
currentNodeSelector :: Selector '[] (Id DOMNode)
currentNodeSelector = mkSelector "currentNode"

-- | @Selector@ for @setCurrentNode:@
setCurrentNodeSelector :: Selector '[Id DOMNode] ()
setCurrentNodeSelector = mkSelector "setCurrentNode:"

