{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Set	***************
--
-- Generated bindings for @NSSet@.
module ObjC.AppKit.NSSet
  ( NSSet
  , IsNSSet(..)
  , setWithCollectionViewIndexPath
  , setWithCollectionViewIndexPaths
  , enumerateIndexPathsWithOptions_usingBlock
  , setWithCollectionViewIndexPathSelector
  , setWithCollectionViewIndexPathsSelector
  , enumerateIndexPathsWithOptions_usingBlockSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ setWithCollectionViewIndexPath:@
setWithCollectionViewIndexPath :: IsNSIndexPath indexPath => indexPath -> IO (Id NSSet)
setWithCollectionViewIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSSet"
    withObjCPtr indexPath $ \raw_indexPath ->
      sendClassMsg cls' (mkSelector "setWithCollectionViewIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setWithCollectionViewIndexPaths:@
setWithCollectionViewIndexPaths :: IsNSArray indexPaths => indexPaths -> IO (Id NSSet)
setWithCollectionViewIndexPaths indexPaths =
  do
    cls' <- getRequiredClass "NSSet"
    withObjCPtr indexPaths $ \raw_indexPaths ->
      sendClassMsg cls' (mkSelector "setWithCollectionViewIndexPaths:") (retPtr retVoid) [argPtr (castPtr raw_indexPaths :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlock :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexPathsWithOptions_usingBlock nsSet  opts block =
  sendMsg nsSet (mkSelector "enumerateIndexPathsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setWithCollectionViewIndexPath:@
setWithCollectionViewIndexPathSelector :: Selector
setWithCollectionViewIndexPathSelector = mkSelector "setWithCollectionViewIndexPath:"

-- | @Selector@ for @setWithCollectionViewIndexPaths:@
setWithCollectionViewIndexPathsSelector :: Selector
setWithCollectionViewIndexPathsSelector = mkSelector "setWithCollectionViewIndexPaths:"

-- | @Selector@ for @enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlockSelector :: Selector
enumerateIndexPathsWithOptions_usingBlockSelector = mkSelector "enumerateIndexPathsWithOptions:usingBlock:"

