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
  , parameterWithAddressSelector
  , parameterWithID_scope_elementSelector
  , createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector
  , createGroupWithIdentifier_name_childrenSelector
  , createGroupTemplateSelector
  , createGroupFromTemplate_identifier_name_addressOffsetSelector
  , createTreeWithChildrenSelector


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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | parameterWithAddress:
--
-- Search a tree for a parameter with a specific address.
--
-- Returns: The parameter corresponding to the supplied address, or nil if no such parameter exists.
--
-- ObjC selector: @- parameterWithAddress:@
parameterWithAddress :: IsAUParameterTree auParameterTree => auParameterTree -> CULong -> IO (Id AUParameter)
parameterWithAddress auParameterTree  address =
  sendMsg auParameterTree (mkSelector "parameterWithAddress:") (retPtr retVoid) [argCULong (fromIntegral address)] >>= retainedObject . castPtr

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
parameterWithID_scope_element auParameterTree  paramID scope element =
  sendMsg auParameterTree (mkSelector "parameterWithID:scope:element:") (retPtr retVoid) [argCUInt (fromIntegral paramID), argCUInt (fromIntegral scope), argCUInt (fromIntegral element)] >>= retainedObject . castPtr

-- | Create an AUParameter. See AUParameter's properties for descriptions of the arguments.
--
-- ObjC selector: @+ createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:@
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParameters :: (IsNSString identifier, IsNSString name, IsNSString unitName, IsNSArray valueStrings, IsNSArray dependentParameters) => identifier -> name -> CULong -> CFloat -> CFloat -> AudioUnitParameterUnit -> unitName -> AudioUnitParameterOptions -> valueStrings -> dependentParameters -> IO (Id AUParameter)
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParameters identifier name address min_ max_ unit unitName flags valueStrings dependentParameters =
  do
    cls' <- getRequiredClass "AUParameterTree"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr name $ \raw_name ->
        withObjCPtr unitName $ \raw_unitName ->
          withObjCPtr valueStrings $ \raw_valueStrings ->
            withObjCPtr dependentParameters $ \raw_dependentParameters ->
              sendClassMsg cls' (mkSelector "createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCULong (fromIntegral address), argCFloat (fromIntegral min_), argCFloat (fromIntegral max_), argCUInt (coerce unit), argPtr (castPtr raw_unitName :: Ptr ()), argCUInt (coerce flags), argPtr (castPtr raw_valueStrings :: Ptr ()), argPtr (castPtr raw_dependentParameters :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr name $ \raw_name ->
        withObjCPtr children $ \raw_children ->
          sendClassMsg cls' (mkSelector "createGroupWithIdentifier:name:children:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_children :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr children $ \raw_children ->
      sendClassMsg cls' (mkSelector "createGroupTemplate:") (retPtr retVoid) [argPtr (castPtr raw_children :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr templateGroup $ \raw_templateGroup ->
      withObjCPtr identifier $ \raw_identifier ->
        withObjCPtr name $ \raw_name ->
          sendClassMsg cls' (mkSelector "createGroupFromTemplate:identifier:name:addressOffset:") (retPtr retVoid) [argPtr (castPtr raw_templateGroup :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCULong (fromIntegral addressOffset)] >>= retainedObject . castPtr

-- | Create an AUParameterTree.
--
-- @children@ — The tree's top-level child nodes.
--
-- ObjC selector: @+ createTreeWithChildren:@
createTreeWithChildren :: IsNSArray children => children -> IO (Id AUParameterTree)
createTreeWithChildren children =
  do
    cls' <- getRequiredClass "AUParameterTree"
    withObjCPtr children $ \raw_children ->
      sendClassMsg cls' (mkSelector "createTreeWithChildren:") (retPtr retVoid) [argPtr (castPtr raw_children :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterWithAddress:@
parameterWithAddressSelector :: Selector
parameterWithAddressSelector = mkSelector "parameterWithAddress:"

-- | @Selector@ for @parameterWithID:scope:element:@
parameterWithID_scope_elementSelector :: Selector
parameterWithID_scope_elementSelector = mkSelector "parameterWithID:scope:element:"

-- | @Selector@ for @createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:@
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector :: Selector
createParameterWithIdentifier_name_address_min_max_unit_unitName_flags_valueStrings_dependentParametersSelector = mkSelector "createParameterWithIdentifier:name:address:min:max:unit:unitName:flags:valueStrings:dependentParameters:"

-- | @Selector@ for @createGroupWithIdentifier:name:children:@
createGroupWithIdentifier_name_childrenSelector :: Selector
createGroupWithIdentifier_name_childrenSelector = mkSelector "createGroupWithIdentifier:name:children:"

-- | @Selector@ for @createGroupTemplate:@
createGroupTemplateSelector :: Selector
createGroupTemplateSelector = mkSelector "createGroupTemplate:"

-- | @Selector@ for @createGroupFromTemplate:identifier:name:addressOffset:@
createGroupFromTemplate_identifier_name_addressOffsetSelector :: Selector
createGroupFromTemplate_identifier_name_addressOffsetSelector = mkSelector "createGroupFromTemplate:identifier:name:addressOffset:"

-- | @Selector@ for @createTreeWithChildren:@
createTreeWithChildrenSelector :: Selector
createTreeWithChildrenSelector = mkSelector "createTreeWithChildren:"

