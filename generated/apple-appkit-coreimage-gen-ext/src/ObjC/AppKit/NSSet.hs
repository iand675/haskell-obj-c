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
  , enumerateIndexPathsWithOptions_usingBlock
  , setWithCollectionViewIndexPathSelector
  , enumerateIndexPathsWithOptions_usingBlockSelector


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

-- | @+ setWithCollectionViewIndexPath:@
setWithCollectionViewIndexPath :: RawId -> IO (Id NSSet)
setWithCollectionViewIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSSet"
    sendClassMsg cls' (mkSelector "setWithCollectionViewIndexPath:") (retPtr retVoid) [argPtr (castPtr (unRawId indexPath) :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlock :: IsNSSet nsSet => nsSet -> CInt -> Ptr () -> IO ()
enumerateIndexPathsWithOptions_usingBlock nsSet  opts block =
    sendMsg nsSet (mkSelector "enumerateIndexPathsWithOptions:usingBlock:") retVoid [argCInt (fromIntegral opts), argPtr (castPtr block :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setWithCollectionViewIndexPath:@
setWithCollectionViewIndexPathSelector :: Selector
setWithCollectionViewIndexPathSelector = mkSelector "setWithCollectionViewIndexPath:"

-- | @Selector@ for @enumerateIndexPathsWithOptions:usingBlock:@
enumerateIndexPathsWithOptions_usingBlockSelector :: Selector
enumerateIndexPathsWithOptions_usingBlockSelector = mkSelector "enumerateIndexPathsWithOptions:usingBlock:"

