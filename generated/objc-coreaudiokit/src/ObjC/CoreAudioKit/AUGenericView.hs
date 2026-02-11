{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUGenericView
--
-- An AUGenericView object retrieves and instantiates a generic user interface view for the given audio unit
--
-- Generated bindings for @AUGenericView@.
module ObjC.CoreAudioKit.AUGenericView
  ( AUGenericView
  , IsAUGenericView(..)
  , initWithAudioUnit
  , initWithAudioUnit_displayFlags
  , audioUnit
  , showsExpertParameters
  , setShowsExpertParameters
  , initWithAudioUnitSelector
  , initWithAudioUnit_displayFlagsSelector
  , audioUnitSelector
  , showsExpertParametersSelector
  , setShowsExpertParametersSelector

  -- * Enum types
  , AUGenericViewDisplayFlags(AUGenericViewDisplayFlags)
  , pattern AUViewTitleDisplayFlag
  , pattern AUViewPropertiesDisplayFlag
  , pattern AUViewParametersDisplayFlag

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

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.CoreAudioKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithAudioUnit:
--
-- initializer used to create the view for a specific audio unit
--
-- @au@ — The Audio Unit associated with the view
--
-- Returns: Returns the newly created view object
--
-- ObjC selector: @- initWithAudioUnit:@
initWithAudioUnit :: IsAUGenericView auGenericView => auGenericView -> Ptr () -> IO (Id AUGenericView)
initWithAudioUnit auGenericView  au =
  sendMsg auGenericView (mkSelector "initWithAudioUnit:") (retPtr retVoid) [argPtr au] >>= ownedObject . castPtr

-- | initWithAudioUnit:displayFlags:
--
-- initializer used to create the view for a specific audio unit with a parameter for view flags
--
-- @au@ — The Audio Unit associated with the view
--
-- @inFlags@ — The flags specifying display properties (multiple flags can be combined using the or '|' operator)
--
-- Returns: Returns the newly created view object
--
-- ObjC selector: @- initWithAudioUnit:displayFlags:@
initWithAudioUnit_displayFlags :: IsAUGenericView auGenericView => auGenericView -> Ptr () -> AUGenericViewDisplayFlags -> IO (Id AUGenericView)
initWithAudioUnit_displayFlags auGenericView  inAudioUnit inFlags =
  sendMsg auGenericView (mkSelector "initWithAudioUnit:displayFlags:") (retPtr retVoid) [argPtr inAudioUnit, argCUInt (coerce inFlags)] >>= ownedObject . castPtr

-- | @- audioUnit@
audioUnit :: IsAUGenericView auGenericView => auGenericView -> IO (Ptr ())
audioUnit auGenericView  =
  fmap castPtr $ sendMsg auGenericView (mkSelector "audioUnit") (retPtr retVoid) []

-- | @- showsExpertParameters@
showsExpertParameters :: IsAUGenericView auGenericView => auGenericView -> IO Bool
showsExpertParameters auGenericView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg auGenericView (mkSelector "showsExpertParameters") retCULong []

-- | @- setShowsExpertParameters:@
setShowsExpertParameters :: IsAUGenericView auGenericView => auGenericView -> Bool -> IO ()
setShowsExpertParameters auGenericView  value =
  sendMsg auGenericView (mkSelector "setShowsExpertParameters:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioUnit:@
initWithAudioUnitSelector :: Selector
initWithAudioUnitSelector = mkSelector "initWithAudioUnit:"

-- | @Selector@ for @initWithAudioUnit:displayFlags:@
initWithAudioUnit_displayFlagsSelector :: Selector
initWithAudioUnit_displayFlagsSelector = mkSelector "initWithAudioUnit:displayFlags:"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @showsExpertParameters@
showsExpertParametersSelector :: Selector
showsExpertParametersSelector = mkSelector "showsExpertParameters"

-- | @Selector@ for @setShowsExpertParameters:@
setShowsExpertParametersSelector :: Selector
setShowsExpertParametersSelector = mkSelector "setShowsExpertParameters:"

