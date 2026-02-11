{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CHHapticPattern
--
-- A set of one or more haptic events and/or Dynamic parameters/parameter curves.
--
-- The passed-in arrays' contents are not owned by the pattern object.  Changes made to those arrays		after a CHHapticPattern object is created have no effect on that object.
--
-- Generated bindings for @CHHapticPattern@.
module ObjC.CoreHaptics.CHHapticPattern
  ( CHHapticPattern
  , IsCHHapticPattern(..)
  , init_
  , initWithEvents_parameters_error
  , initWithEvents_parameterCurves_error
  , initWithDictionary_error
  , initWithContentsOfURL_error
  , exportDictionaryAndReturnError
  , duration
  , initSelector
  , initWithEvents_parameters_errorSelector
  , initWithEvents_parameterCurves_errorSelector
  , initWithDictionary_errorSelector
  , initWithContentsOfURL_errorSelector
  , exportDictionaryAndReturnErrorSelector
  , durationSelector


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

import ObjC.CoreHaptics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCHHapticPattern chHapticPattern => chHapticPattern -> IO (Id CHHapticPattern)
init_ chHapticPattern  =
  sendMsg chHapticPattern (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithEvents:parameters:error
--
-- Initialize a new CHHapticPattern.
--
-- @events@ — An NSArray of CHHapticEvents.  Can be empty.
--
-- @parameters@ — An NSArray of CHHapticDynamicParameters.  Can be empty.
--
-- ObjC selector: @- initWithEvents:parameters:error:@
initWithEvents_parameters_error :: (IsCHHapticPattern chHapticPattern, IsNSArray events, IsNSArray parameters, IsNSError outError) => chHapticPattern -> events -> parameters -> outError -> IO (Id CHHapticPattern)
initWithEvents_parameters_error chHapticPattern  events parameters outError =
withObjCPtr events $ \raw_events ->
  withObjCPtr parameters $ \raw_parameters ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg chHapticPattern (mkSelector "initWithEvents:parameters:error:") (retPtr retVoid) [argPtr (castPtr raw_events :: Ptr ()), argPtr (castPtr raw_parameters :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithEvents:parameterCurves:error
--
-- Initialize a new CHHapticPattern with parameters modulated by parameter curves.
--
-- @events@ — An NSArray of CHHapticEvents.  Can be empty.
--
-- @parameterCurves@ — An NSArray of CHHapticParameterCurves.  Can be empty.
--
-- ObjC selector: @- initWithEvents:parameterCurves:error:@
initWithEvents_parameterCurves_error :: (IsCHHapticPattern chHapticPattern, IsNSArray events, IsNSArray parameterCurves, IsNSError outError) => chHapticPattern -> events -> parameterCurves -> outError -> IO (Id CHHapticPattern)
initWithEvents_parameterCurves_error chHapticPattern  events parameterCurves outError =
withObjCPtr events $ \raw_events ->
  withObjCPtr parameterCurves $ \raw_parameterCurves ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg chHapticPattern (mkSelector "initWithEvents:parameterCurves:error:") (retPtr retVoid) [argPtr (castPtr raw_events :: Ptr ()), argPtr (castPtr raw_parameterCurves :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithDictionary:error
--
-- Initialize a new CHHapticPattern using the passed-in NSDictionary.
--
-- @patternDict@ — NSDictionary containing a pattern property list.
--
-- ObjC selector: @- initWithDictionary:error:@
initWithDictionary_error :: (IsCHHapticPattern chHapticPattern, IsNSDictionary patternDict, IsNSError outError) => chHapticPattern -> patternDict -> outError -> IO (Id CHHapticPattern)
initWithDictionary_error chHapticPattern  patternDict outError =
withObjCPtr patternDict $ \raw_patternDict ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg chHapticPattern (mkSelector "initWithDictionary:error:") (retPtr retVoid) [argPtr (castPtr raw_patternDict :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | initWithContentsOfURL:error
--
-- Initialize a new CHHapticPattern using the contents of the passed-in NSURL.
--
-- @ahapURL@ — NSURL of an ahap file.
--
-- This URL must reference a valid AHAP file.
--
-- ObjC selector: @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsCHHapticPattern chHapticPattern, IsNSURL ahapURL, IsNSError outError) => chHapticPattern -> ahapURL -> outError -> IO (Id CHHapticPattern)
initWithContentsOfURL_error chHapticPattern  ahapURL outError =
withObjCPtr ahapURL $ \raw_ahapURL ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg chHapticPattern (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_ahapURL :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | exportDictionaryAndReturnError:error
--
-- Returns a NSDictionary representation of the contents of the pattern.
--
-- Patterns containing custom audio resource IDs cannot be exported and will return nil        with the error code set to CHHapticErrorCodeOperationNotPermitted.
--
-- ObjC selector: @- exportDictionaryAndReturnError:@
exportDictionaryAndReturnError :: (IsCHHapticPattern chHapticPattern, IsNSError outError) => chHapticPattern -> outError -> IO (Id NSDictionary)
exportDictionaryAndReturnError chHapticPattern  outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg chHapticPattern (mkSelector "exportDictionaryAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | duration
--
-- Pattern duration is calculated as the start time of the pattern's last event or parameter, plus that event's duration if present.
--
-- ObjC selector: @- duration@
duration :: IsCHHapticPattern chHapticPattern => chHapticPattern -> IO CDouble
duration chHapticPattern  =
  sendMsg chHapticPattern (mkSelector "duration") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEvents:parameters:error:@
initWithEvents_parameters_errorSelector :: Selector
initWithEvents_parameters_errorSelector = mkSelector "initWithEvents:parameters:error:"

-- | @Selector@ for @initWithEvents:parameterCurves:error:@
initWithEvents_parameterCurves_errorSelector :: Selector
initWithEvents_parameterCurves_errorSelector = mkSelector "initWithEvents:parameterCurves:error:"

-- | @Selector@ for @initWithDictionary:error:@
initWithDictionary_errorSelector :: Selector
initWithDictionary_errorSelector = mkSelector "initWithDictionary:error:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @exportDictionaryAndReturnError:@
exportDictionaryAndReturnErrorSelector :: Selector
exportDictionaryAndReturnErrorSelector = mkSelector "exportDictionaryAndReturnError:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

