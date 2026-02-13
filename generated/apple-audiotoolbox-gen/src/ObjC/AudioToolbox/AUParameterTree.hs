{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUParameterTree
--
-- The top level group node, representing all of an audio unit's parameters.
--
-- An audio unit's parameters are organized into a tree containing groups and parameters.		Groups may be nested.
--
-- The tree is KVO-compliant. For example, if the tree contains a group named "LFO" ,		containing a parameter named rate, then "LFO.rate" refers to that parameter object, and		"LFO.rate.value" refers to that parameter's value.
--
-- An audio unit may choose to dynamically rearrange the tree. When doing so, it must		issue a KVO notification on the audio unit's parameterTree property. The tree's elements are		mostly immutable (except for values and implementor hooks); the only way to modify them		is to publish a new tree.
--
-- Generated bindings for @AUParameterTree@.
module ObjC.AudioToolbox.AUParameterTree
  ( AUParameterTree
  , IsAUParameterTree(..)
  , parameterWithAddress
  , parameterWithID_scope_element
  , createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParameters
  , createGroupWithIdentifier_name_children
  , createGroupTemplate
  , createGroupFromTemplate_identifier_name_addressOffset
  , createTreeWithChildren
  , createGroupFromTemplate_identifier_name_addressOffsetSelector
  , createGroupTemplateSelector
  , createGroupWithIdentifier_name_childrenSelector
  , createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector
  , createTreeWithChildrenSelector
  , parameterWithAddressSelector
  , parameterWithID_scope_elementSelector

  -- * Enum types
  , AudioUnitParameterOptions(AudioUnitParameterOptions)
  , pattern KAudioUnitParameterFlag_CFNameRelease
  , pattern KAudioUnitParameterFlag_OmitFromPresets
  , pattern KAudioUnitParameterFlag_PlotHistory
  , pattern KAudioUnitParameterFlag_MeterReadOnly
  , pattern KAudioUnitParameterFlag_DisplayMask
  , pattern KAudioUnitParameterFlag_DisplaySquareRoot
  , pattern KAudioUnitParameterFlag_DisplaySquared
  , pattern KAudioUnitParameterFlag_DisplayCubed
  , pattern KAudioUnitParameterFlag_DisplayCubeRoot
  , pattern KAudioUnitParameterFlag_DisplayExponential
  , pattern KAudioUnitParameterFlag_HasClump
  , pattern KAudioUnitParameterFlag_ValuesHaveStrings
  , pattern KAudioUnitParameterFlag_DisplayLogarithmic
  , pattern KAudioUnitParameterFlag_IsHighResolution
  , pattern KAudioUnitParameterFlag_NonRealTime
  , pattern KAudioUnitParameterFlag_CanRamp
  , pattern KAudioUnitParameterFlag_ExpertMode
  , pattern KAudioUnitParameterFlag_HasCFNameString
  , pattern KAudioUnitParameterFlag_IsGlobalMeta
  , pattern KAudioUnitParameterFlag_IsElementMeta
  , pattern KAudioUnitParameterFlag_IsReadable
  , pattern KAudioUnitParameterFlag_IsWritable
  , AudioUnitParameterUnit(AudioUnitParameterUnit)
  , pattern KAudioUnitParameterUnit_Generic
  , pattern KAudioUnitParameterUnit_Indexed
  , pattern KAudioUnitParameterUnit_Boolean
  , pattern KAudioUnitParameterUnit_Percent
  , pattern KAudioUnitParameterUnit_Seconds
  , pattern KAudioUnitParameterUnit_SampleFrames
  , pattern KAudioUnitParameterUnit_Phase
  , pattern KAudioUnitParameterUnit_Rate
  , pattern KAudioUnitParameterUnit_Hertz
  , pattern KAudioUnitParameterUnit_Cents
  , pattern KAudioUnitParameterUnit_RelativeSemiTones
  , pattern KAudioUnitParameterUnit_MIDINoteNumber
  , pattern KAudioUnitParameterUnit_MIDIController
  , pattern KAudioUnitParameterUnit_Decibels
  , pattern KAudioUnitParameterUnit_LinearGain
  , pattern KAudioUnitParameterUnit_Degrees
  , pattern KAudioUnitParameterUnit_EqualPowerCrossfade
  , pattern KAudioUnitParameterUnit_MixerFaderCurve1
  , pattern KAudioUnitParameterUnit_Pan
  , pattern KAudioUnitParameterUnit_Meters
  , pattern KAudioUnitParameterUnit_AbsoluteCents
  , pattern KAudioUnitParameterUnit_Octaves
  , pattern KAudioUnitParameterUnit_BPM
  , pattern KAudioUnitParameterUnit_Beats
  , pattern KAudioUnitParameterUnit_Milliseconds
  , pattern KAudioUnitParameterUnit_Ratio
  , pattern KAudioUnitParameterUnit_CustomUnit
  , pattern KAudioUnitParameterUnit_MIDI2Controller

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.AudioToolbox.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | parameterWithAddress:
--
-- Search a tree for a parameter with a specific address.
--
-- Returns: The parameter corresponding to the supplied address, or nil if no such parameter exists.
--
-- ObjC selector: @- parameterWithAddress:@
parameterWithAddress :: IsAUParameterTree auParameterTree => auParameterTree -> CULong -> IO (Id AUParameter)
parameterWithAddress auParameterTree address =
  sendMessage auParameterTree parameterWithAddressSelector address

-- | parameterWithID:scope:element:
--
-- Search a tree for a specific v2 audio unit parameter.
--
-- V2 audio units publish parameters identified by a parameter ID, scope, and element.		A host that knows that it is dealing with a v2 unit can locate parameters using this method,		for example, for the Apple-supplied system audio units.
--
-- Returns: The parameter corresponding to the supplied ID/scope/element, or nil if no such parameter		exists, or if the audio unit is not a v2 unit.
--
-- ObjC selector: @- parameterWithID:scope:element:@
parameterWithID_scope_element :: IsAUParameterTree auParameterTree => auParameterTree -> CUInt -> CUInt -> CUInt -> IO (Id AUParameter)
parameterWithID_scope_element auParameterTree paramID scope element =
  sendMessage auParameterTree parameterWithID_scope_elementSelector paramID scope element

-- | Create an AUParameter. See AUParameter's properties for descriptions of the arguments.
--
-- ObjC selector: @+ createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:@
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParameters :: (IsNSString identifier, IsNSString name, IsNSString unitName, IsNSArray valueStrings, IsNSArray dependentParameters) => identifier -> name -> CULong -> CFloat -> CFloat -> AudioUnitParameterUnit -> unitName -> AudioUnitParameterOptions -> valueStrings -> dependentParameters -> IO (Id AUParameter)
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParameters identifier name address min_ max_ unit unitName flags valueStrings dependentParameters =
  do
    cls' <- getRequiredClass "AUParameterTree"
    sendClassMessage cls' createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector (toNSString identifier) (toNSString name) address min_ max_ unit (toNSString unitName) flags (toNSArray valueStrings) (toNSArray dependentParameters)

-- | Create an AUParameterGroup.
--
-- @identifier@ — An identifier for the group (non-localized, persistent).
--
-- @name@ — The group's human-readable name (localized).
--
-- @children@ — The group's child nodes.
--
-- ObjC selector: @+ createGroupWithIdentifier:name:children:@
createGroupWithIdentifier_name_children :: (IsNSString identifier, IsNSString name, IsNSArray children) => identifier -> name -> children -> IO (Id AUParameterGroup)
createGroupWithIdentifier_name_children identifier name children =
  do
    cls' <- getRequiredClass "AUParameterTree"
    sendClassMessage cls' createGroupWithIdentifier_name_childrenSelector (toNSString identifier) (toNSString name) (toNSArray children)

-- | Create a template group which may be used as a prototype for further group instances.
--
-- Template groups provide a way to construct multiple instances of identical parameter		groups, sharing certain immutable state between the instances.
--
-- Template groups may not appear in trees except at the root.
--
-- ObjC selector: @+ createGroupTemplate:@
createGroupTemplate :: IsNSArray children => children -> IO (Id AUParameterGroup)
createGroupTemplate children =
  do
    cls' <- getRequiredClass "AUParameterTree"
    sendClassMessage cls' createGroupTemplateSelector (toNSArray children)

-- | Initialize a group as a copied instance of a template group.
--
-- @templateGroup@ — A group to be used as a template and largely copied.
--
-- @identifier@ — An identifier for the new group (non-localized, persistent).
--
-- @name@ — The new group's human-readable name (localized).
--
-- @addressOffset@ — The new group's parameters' addresses will be offset from those in							the template by this value.
--
-- ObjC selector: @+ createGroupFromTemplate:identifier:name:addressOffset:@
createGroupFromTemplate_identifier_name_addressOffset :: (IsAUParameterGroup templateGroup, IsNSString identifier, IsNSString name) => templateGroup -> identifier -> name -> CULong -> IO (Id AUParameterGroup)
createGroupFromTemplate_identifier_name_addressOffset templateGroup identifier name addressOffset =
  do
    cls' <- getRequiredClass "AUParameterTree"
    sendClassMessage cls' createGroupFromTemplate_identifier_name_addressOffsetSelector (toAUParameterGroup templateGroup) (toNSString identifier) (toNSString name) addressOffset

-- | Create an AUParameterTree.
--
-- @children@ — The tree's top-level child nodes.
--
-- ObjC selector: @+ createTreeWithChildren:@
createTreeWithChildren :: IsNSArray children => children -> IO (Id AUParameterTree)
createTreeWithChildren children =
  do
    cls' <- getRequiredClass "AUParameterTree"
    sendClassMessage cls' createTreeWithChildrenSelector (toNSArray children)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterWithAddress:@
parameterWithAddressSelector :: Selector '[CULong] (Id AUParameter)
parameterWithAddressSelector = mkSelector "parameterWithAddress:"

-- | @Selector@ for @parameterWithID:scope:element:@
parameterWithID_scope_elementSelector :: Selector '[CUInt, CUInt, CUInt] (Id AUParameter)
parameterWithID_scope_elementSelector = mkSelector "parameterWithID:scope:element:"

-- | @Selector@ for @createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:@
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector :: Selector '[Id NSString, Id NSString, CULong, CFloat, CFloat, AudioUnitParameterUnit, Id NSString, AudioUnitParameterOptions, Id NSArray, Id NSArray] (Id AUParameter)
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector = mkSelector "createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:"

-- | @Selector@ for @createGroupWithIdentifier:name:children:@
createGroupWithIdentifier_name_childrenSelector :: Selector '[Id NSString, Id NSString, Id NSArray] (Id AUParameterGroup)
createGroupWithIdentifier_name_childrenSelector = mkSelector "createGroupWithIdentifier:name:children:"

-- | @Selector@ for @createGroupTemplate:@
createGroupTemplateSelector :: Selector '[Id NSArray] (Id AUParameterGroup)
createGroupTemplateSelector = mkSelector "createGroupTemplate:"

-- | @Selector@ for @createGroupFromTemplate:identifier:name:addressOffset:@
createGroupFromTemplate_identifier_name_addressOffsetSelector :: Selector '[Id AUParameterGroup, Id NSString, Id NSString, CULong] (Id AUParameterGroup)
createGroupFromTemplate_identifier_name_addressOffsetSelector = mkSelector "createGroupFromTemplate:identifier:name:addressOffset:"

-- | @Selector@ for @createTreeWithChildren:@
createTreeWithChildrenSelector :: Selector '[Id NSArray] (Id AUParameterTree)
createTreeWithChildrenSelector = mkSelector "createTreeWithChildren:"

