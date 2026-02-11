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
  , expandEntityReferences
  , referenceNode
  , pointerBeforeReferenceNode
  , nextNodeSelector
  , previousNodeSelector
  , detachSelector
  , rootSelector
  , whatToShowSelector
  , expandEntityReferencesSelector
  , referenceNodeSelector
  , pointerBeforeReferenceNodeSelector


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

-- | @- nextNode@
nextNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
nextNode domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "nextNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousNode@
previousNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
previousNode domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "previousNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- detach@
detach :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO ()
detach domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "detach") retVoid []

-- | @- root@
root :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
root domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "root") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- whatToShow@
whatToShow :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO CUInt
whatToShow domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "whatToShow") retCUInt []

-- | @- expandEntityReferences@
expandEntityReferences :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO Bool
expandEntityReferences domNodeIterator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNodeIterator (mkSelector "expandEntityReferences") retCULong []

-- | @- referenceNode@
referenceNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO (Id DOMNode)
referenceNode domNodeIterator  =
  sendMsg domNodeIterator (mkSelector "referenceNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pointerBeforeReferenceNode@
pointerBeforeReferenceNode :: IsDOMNodeIterator domNodeIterator => domNodeIterator -> IO Bool
pointerBeforeReferenceNode domNodeIterator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNodeIterator (mkSelector "pointerBeforeReferenceNode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @detach@
detachSelector :: Selector
detachSelector = mkSelector "detach"

-- | @Selector@ for @root@
rootSelector :: Selector
rootSelector = mkSelector "root"

-- | @Selector@ for @whatToShow@
whatToShowSelector :: Selector
whatToShowSelector = mkSelector "whatToShow"

-- | @Selector@ for @expandEntityReferences@
expandEntityReferencesSelector :: Selector
expandEntityReferencesSelector = mkSelector "expandEntityReferences"

-- | @Selector@ for @referenceNode@
referenceNodeSelector :: Selector
referenceNodeSelector = mkSelector "referenceNode"

-- | @Selector@ for @pointerBeforeReferenceNode@
pointerBeforeReferenceNodeSelector :: Selector
pointerBeforeReferenceNodeSelector = mkSelector "pointerBeforeReferenceNode"

