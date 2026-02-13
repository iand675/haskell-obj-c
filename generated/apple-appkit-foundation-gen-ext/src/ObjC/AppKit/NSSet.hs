{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , enumerateIndexPathsWithOptions_usingBlockSelector
  , setWithCollectionViewIndexPathSelector
  , setWithCollectionViewIndexPathsSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' setWithCollectionViewIndexPathSelector (toNSIndexPath indexPath)

-- | @+ setWithCollectionViewIndexPaths:@
setWithCollectionViewIndexPaths :: IsNSArray indexPaths => indexPaths -> IO (Id NSSet)
setWithCollectionViewIndexPaths indexPaths =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMessage cls' setWithCollectionViewIndexPathsSelector (toNSArray indexPaths)

-- | @- enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlock :: IsNSSet nsSet => nsSet -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateIndexPathsWithOptions_usingBlock nsSet opts block =
  sendMessage nsSet enumerateIndexPathsWithOptions_usingBlockSelector opts block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setWithCollectionViewIndexPath:@
setWithCollectionViewIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSSet)
setWithCollectionViewIndexPathSelector = mkSelector "setWithCollectionViewIndexPath:"

-- | @Selector@ for @setWithCollectionViewIndexPaths:@
setWithCollectionViewIndexPathsSelector :: Selector '[Id NSArray] (Id NSSet)
setWithCollectionViewIndexPathsSelector = mkSelector "setWithCollectionViewIndexPaths:"

-- | @Selector@ for @enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateIndexPathsWithOptions_usingBlockSelector = mkSelector "enumerateIndexPathsWithOptions:usingBlock:"

