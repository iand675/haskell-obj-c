{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVCompositionTrackSegment@.
module ObjC.AVFoundation.AVCompositionTrackSegment
  ( AVCompositionTrackSegment
  , IsAVCompositionTrackSegment(..)
  , empty
  , sourceURL
  , sourceTrackID
  , emptySelector
  , sourceURLSelector
  , sourceTrackIDSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- empty@
empty :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO Bool
empty avCompositionTrackSegment  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCompositionTrackSegment (mkSelector "empty") retCULong []

-- | @- sourceURL@
sourceURL :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO (Id NSURL)
sourceURL avCompositionTrackSegment  =
  sendMsg avCompositionTrackSegment (mkSelector "sourceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceTrackID@
sourceTrackID :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO CInt
sourceTrackID avCompositionTrackSegment  =
  sendMsg avCompositionTrackSegment (mkSelector "sourceTrackID") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @empty@
emptySelector :: Selector
emptySelector = mkSelector "empty"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector
sourceURLSelector = mkSelector "sourceURL"

-- | @Selector@ for @sourceTrackID@
sourceTrackIDSelector :: Selector
sourceTrackIDSelector = mkSelector "sourceTrackID"

