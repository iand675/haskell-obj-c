{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object representing a single command to be invoked and the response required for the invoke to be considered successful.
--
-- Generated bindings for @MTRCommandWithRequiredResponse@.
module ObjC.Matter.MTRCommandWithRequiredResponse
  ( MTRCommandWithRequiredResponse
  , IsMTRCommandWithRequiredResponse(..)
  , initWithPath_commandFields_requiredResponse
  , path
  , setPath
  , commandFields
  , setCommandFields
  , requiredResponse
  , setRequiredResponse
  , commandFieldsSelector
  , initWithPath_commandFields_requiredResponseSelector
  , pathSelector
  , requiredResponseSelector
  , setCommandFieldsSelector
  , setPathSelector
  , setRequiredResponseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPath:commandFields:requiredResponse:@
initWithPath_commandFields_requiredResponse :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsMTRCommandPath path, IsNSDictionary commandFields, IsNSDictionary requiredResponse) => mtrCommandWithRequiredResponse -> path -> commandFields -> requiredResponse -> IO (Id MTRCommandWithRequiredResponse)
initWithPath_commandFields_requiredResponse mtrCommandWithRequiredResponse path commandFields requiredResponse =
  sendOwnedMessage mtrCommandWithRequiredResponse initWithPath_commandFields_requiredResponseSelector (toMTRCommandPath path) (toNSDictionary commandFields) (toNSDictionary requiredResponse)

-- | The path of the command being invoked.
--
-- ObjC selector: @- path@
path :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id MTRCommandPath)
path mtrCommandWithRequiredResponse =
  sendMessage mtrCommandWithRequiredResponse pathSelector

-- | The path of the command being invoked.
--
-- ObjC selector: @- setPath:@
setPath :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsMTRCommandPath value) => mtrCommandWithRequiredResponse -> value -> IO ()
setPath mtrCommandWithRequiredResponse value =
  sendMessage mtrCommandWithRequiredResponse setPathSelector (toMTRCommandPath value)

-- | The command fields to pass for the command invoke.  nil if this command does not have any fields.  If not nil, this should be a data-value dictionary of MTRStructureValueType.
--
-- ObjC selector: @- commandFields@
commandFields :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id NSDictionary)
commandFields mtrCommandWithRequiredResponse =
  sendMessage mtrCommandWithRequiredResponse commandFieldsSelector

-- | The command fields to pass for the command invoke.  nil if this command does not have any fields.  If not nil, this should be a data-value dictionary of MTRStructureValueType.
--
-- ObjC selector: @- setCommandFields:@
setCommandFields :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsNSDictionary value) => mtrCommandWithRequiredResponse -> value -> IO ()
setCommandFields mtrCommandWithRequiredResponse value =
  sendMessage mtrCommandWithRequiredResponse setCommandFieldsSelector (toNSDictionary value)

-- | The response that represents this command succeeding.
--
-- If this is nil, that indicates that the invoke is considered successful if it does not result in an error status response.
--
-- If this is is not nil, then the invoke is considered successful if the response is a data response and for each entry in the provided requiredResponse the field whose field ID matches the key of the entry has a value that equals the value of the entry.  Values of entries are data-value dictionaries.
--
-- ObjC selector: @- requiredResponse@
requiredResponse :: IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse => mtrCommandWithRequiredResponse -> IO (Id NSDictionary)
requiredResponse mtrCommandWithRequiredResponse =
  sendMessage mtrCommandWithRequiredResponse requiredResponseSelector

-- | The response that represents this command succeeding.
--
-- If this is nil, that indicates that the invoke is considered successful if it does not result in an error status response.
--
-- If this is is not nil, then the invoke is considered successful if the response is a data response and for each entry in the provided requiredResponse the field whose field ID matches the key of the entry has a value that equals the value of the entry.  Values of entries are data-value dictionaries.
--
-- ObjC selector: @- setRequiredResponse:@
setRequiredResponse :: (IsMTRCommandWithRequiredResponse mtrCommandWithRequiredResponse, IsNSDictionary value) => mtrCommandWithRequiredResponse -> value -> IO ()
setRequiredResponse mtrCommandWithRequiredResponse value =
  sendMessage mtrCommandWithRequiredResponse setRequiredResponseSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPath:commandFields:requiredResponse:@
initWithPath_commandFields_requiredResponseSelector :: Selector '[Id MTRCommandPath, Id NSDictionary, Id NSDictionary] (Id MTRCommandWithRequiredResponse)
initWithPath_commandFields_requiredResponseSelector = mkSelector "initWithPath:commandFields:requiredResponse:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id MTRCommandPath)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id MTRCommandPath] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @commandFields@
commandFieldsSelector :: Selector '[] (Id NSDictionary)
commandFieldsSelector = mkSelector "commandFields"

-- | @Selector@ for @setCommandFields:@
setCommandFieldsSelector :: Selector '[Id NSDictionary] ()
setCommandFieldsSelector = mkSelector "setCommandFields:"

-- | @Selector@ for @requiredResponse@
requiredResponseSelector :: Selector '[] (Id NSDictionary)
requiredResponseSelector = mkSelector "requiredResponse"

-- | @Selector@ for @setRequiredResponse:@
setRequiredResponseSelector :: Selector '[Id NSDictionary] ()
setRequiredResponseSelector = mkSelector "setRequiredResponse:"

