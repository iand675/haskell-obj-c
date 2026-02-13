{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMNodeIterator@.
module ObjC.WebKit.DOMNodeIterator
  ( DOMNodeIterator
  , IsDOMNodeIterator(..)
  , nextNode
  , previousNode
  , detach
  , root
  , whatToShow
  , filter_
  , expandEntityReferences
  , referenceNode
  , pointerBeforeReferenceNode
  , detachSelector
  , expandEntityReferencesSelector
  , filterSelector
  , nextNodeSelector
  , pointerBeforeReferenceNodeSelector
  , previousNodeSelector
  , referenceNodeSelector
  , rootSelector
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

-- | @- nextNode@
nextNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
nextNode domNodeIterator =
  sendMessage domNodeIterator nextNodeSelector

-- | @- previousNode@
previousNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
previousNode domNodeIterator =
  sendMessage domNodeIterator previousNodeSelector

-- | @- detach@
detach :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO ()
detach domNodeIterator =
  sendMessage domNodeIterator detachSelector

-- | @- root@
root :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
root domNodeIterator =
  sendMessage domNodeIterator rootSelector

-- | @- whatToShow@
whatToShow :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO CUInt
whatToShow domNodeIterator =
  sendMessage domNodeIterator whatToShowSelector

-- | @- filter@
filter_ :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO RawId
filter_ domNodeIterator =
  sendMessage domNodeIterator filterSelector

-- | @- expandEntityReferences@
expandEntityReferences :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO Bool
expandEntityReferences domNodeIterator =
  sendMessage domNodeIterator expandEntityReferencesSelector

-- | @- referenceNode@
referenceNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
referenceNode domNodeIterator =
  sendMessage domNodeIterator referenceNodeSelector

-- | @- pointerBeforeReferenceNode@
pointerBeforeReferenceNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO Bool
pointerBeforeReferenceNode domNodeIterator =
  sendMessage domNodeIterator pointerBeforeReferenceNodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector '[] (Id DOMNode)
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector '[] (Id DOMNode)
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @detach@
detachSelector :: Selector '[] ()
detachSelector = mkSelector "detach"

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

-- | @Selector@ for @referenceNode@
referenceNodeSelector :: Selector '[] (Id DOMNode)
referenceNodeSelector = mkSelector "referenceNode"

-- | @Selector@ for @pointerBeforeReferenceNode@
pointerBeforeReferenceNodeSelector :: Selector '[] Bool
pointerBeforeReferenceNodeSelector = mkSelector "pointerBeforeReferenceNode"

