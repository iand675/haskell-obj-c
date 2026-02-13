{-# LANGUAGE DataKinds #-}
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
  , sourceTrackIDSelector
  , sourceURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- empty@
empty :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO Bool
empty avCompositionTrackSegment =
  sendMessage avCompositionTrackSegment emptySelector

-- | @- sourceURL@
sourceURL :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO (Id NSURL)
sourceURL avCompositionTrackSegment =
  sendMessage avCompositionTrackSegment sourceURLSelector

-- | @- sourceTrackID@
sourceTrackID :: IsAVCompositionTrackSegment avCompositionTrackSegment => avCompositionTrackSegment -> IO CInt
sourceTrackID avCompositionTrackSegment =
  sendMessage avCompositionTrackSegment sourceTrackIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @empty@
emptySelector :: Selector '[] Bool
emptySelector = mkSelector "empty"

-- | @Selector@ for @sourceURL@
sourceURLSelector :: Selector '[] (Id NSURL)
sourceURLSelector = mkSelector "sourceURL"

-- | @Selector@ for @sourceTrackID@
sourceTrackIDSelector :: Selector '[] CInt
sourceTrackIDSelector = mkSelector "sourceTrackID"

