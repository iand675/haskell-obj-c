{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , audioUnitSelector
  , initWithAudioUnitSelector
  , initWithAudioUnit_displayFlagsSelector
  , setShowsExpertParametersSelector
  , showsExpertParametersSelector

  -- * Enum types
  , AUGenericViewDisplayFlags(AUGenericViewDisplayFlags)
  , pattern AUViewTitleDisplayFlag
  , pattern AUViewPropertiesDisplayFlag
  , pattern AUViewParametersDisplayFlag

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithAudioUnit auGenericView au =
  sendOwnedMessage auGenericView initWithAudioUnitSelector au

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
initWithAudioUnit_displayFlags auGenericView inAudioUnit inFlags =
  sendOwnedMessage auGenericView initWithAudioUnit_displayFlagsSelector inAudioUnit inFlags

-- | @- audioUnit@
audioUnit :: IsAUGenericView auGenericView => auGenericView -> IO (Ptr ())
audioUnit auGenericView =
  sendMessage auGenericView audioUnitSelector

-- | @- showsExpertParameters@
showsExpertParameters :: IsAUGenericView auGenericView => auGenericView -> IO Bool
showsExpertParameters auGenericView =
  sendMessage auGenericView showsExpertParametersSelector

-- | @- setShowsExpertParameters:@
setShowsExpertParameters :: IsAUGenericView auGenericView => auGenericView -> Bool -> IO ()
setShowsExpertParameters auGenericView value =
  sendMessage auGenericView setShowsExpertParametersSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAudioUnit:@
initWithAudioUnitSelector :: Selector '[Ptr ()] (Id AUGenericView)
initWithAudioUnitSelector = mkSelector "initWithAudioUnit:"

-- | @Selector@ for @initWithAudioUnit:displayFlags:@
initWithAudioUnit_displayFlagsSelector :: Selector '[Ptr (), AUGenericViewDisplayFlags] (Id AUGenericView)
initWithAudioUnit_displayFlagsSelector = mkSelector "initWithAudioUnit:displayFlags:"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector '[] (Ptr ())
audioUnitSelector = mkSelector "audioUnit"

-- | @Selector@ for @showsExpertParameters@
showsExpertParametersSelector :: Selector '[] Bool
showsExpertParametersSelector = mkSelector "showsExpertParameters"

-- | @Selector@ for @setShowsExpertParameters:@
setShowsExpertParametersSelector :: Selector '[Bool] ()
setShowsExpertParametersSelector = mkSelector "setShowsExpertParameters:"

