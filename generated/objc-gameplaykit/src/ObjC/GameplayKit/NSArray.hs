{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Immutable Array		***************
--
-- Generated bindings for @NSArray@.
module ObjC.GameplayKit.NSArray
  ( NSArray
  , IsNSArray(..)
  , shuffledArrayWithRandomSource
  , shuffledArray
  , shuffledArrayWithRandomSourceSelector
  , shuffledArraySelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- shuffledArrayWithRandomSource:@
shuffledArrayWithRandomSource :: (IsNSArray nsArray, IsGKRandomSource randomSource) => nsArray -> randomSource -> IO (Id NSArray)
shuffledArrayWithRandomSource nsArray  randomSource =
withObjCPtr randomSource $ \raw_randomSource ->
    sendMsg nsArray (mkSelector "shuffledArrayWithRandomSource:") (retPtr retVoid) [argPtr (castPtr raw_randomSource :: Ptr ())] >>= retainedObject . castPtr

-- | @- shuffledArray@
shuffledArray :: IsNSArray nsArray => nsArray -> IO (Id NSArray)
shuffledArray nsArray  =
  sendMsg nsArray (mkSelector "shuffledArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shuffledArrayWithRandomSource:@
shuffledArrayWithRandomSourceSelector :: Selector
shuffledArrayWithRandomSourceSelector = mkSelector "shuffledArrayWithRandomSource:"

-- | @Selector@ for @shuffledArray@
shuffledArraySelector :: Selector
shuffledArraySelector = mkSelector "shuffledArray"

