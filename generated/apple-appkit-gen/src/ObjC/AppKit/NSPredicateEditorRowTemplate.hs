{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPredicateEditorRowTemplate@.
module ObjC.AppKit.NSPredicateEditorRowTemplate
  ( NSPredicateEditorRowTemplate
  , IsNSPredicateEditorRowTemplate(..)
  , matchForPredicate
  , setPredicate
  , predicateWithSubpredicates
  , displayableSubpredicatesOfPredicate
  , initWithLeftExpressions_rightExpressions_modifier_operators_options
  , initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options
  , initWithCompoundTypes
  , templatesWithAttributeKeyPaths_inEntityDescription
  , templateViews
  , leftExpressions
  , rightExpressions
  , rightExpressionAttributeType
  , modifier
  , operators
  , options
  , compoundTypes
  , compoundTypesSelector
  , displayableSubpredicatesOfPredicateSelector
  , initWithCompoundTypesSelector
  , initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector
  , initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector
  , leftExpressionsSelector
  , matchForPredicateSelector
  , modifierSelector
  , operatorsSelector
  , optionsSelector
  , predicateWithSubpredicatesSelector
  , rightExpressionAttributeTypeSelector
  , rightExpressionsSelector
  , setPredicateSelector
  , templateViewsSelector
  , templatesWithAttributeKeyPaths_inEntityDescriptionSelector

  -- * Enum types
  , NSAttributeType(NSAttributeType)
  , pattern NSUndefinedAttributeType
  , pattern NSInteger16AttributeType
  , pattern NSInteger32AttributeType
  , pattern NSInteger64AttributeType
  , pattern NSDecimalAttributeType
  , pattern NSDoubleAttributeType
  , pattern NSFloatAttributeType
  , pattern NSStringAttributeType
  , pattern NSBooleanAttributeType
  , pattern NSDateAttributeType
  , pattern NSBinaryDataAttributeType
  , pattern NSUUIDAttributeType
  , pattern NSURIAttributeType
  , pattern NSTransformableAttributeType
  , pattern NSObjectIDAttributeType
  , pattern NSCompositeAttributeType
  , NSComparisonPredicateModifier(NSComparisonPredicateModifier)
  , pattern NSDirectPredicateModifier
  , pattern NSAllPredicateModifier
  , pattern NSAnyPredicateModifier

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- matchForPredicate:@
matchForPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO CDouble
matchForPredicate nsPredicateEditorRowTemplate predicate =
  sendMessage nsPredicateEditorRowTemplate matchForPredicateSelector (toNSPredicate predicate)

-- | @- setPredicate:@
setPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO ()
setPredicate nsPredicateEditorRowTemplate predicate =
  sendMessage nsPredicateEditorRowTemplate setPredicateSelector (toNSPredicate predicate)

-- | @- predicateWithSubpredicates:@
predicateWithSubpredicates :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray subpredicates) => nsPredicateEditorRowTemplate -> subpredicates -> IO (Id NSPredicate)
predicateWithSubpredicates nsPredicateEditorRowTemplate subpredicates =
  sendMessage nsPredicateEditorRowTemplate predicateWithSubpredicatesSelector (toNSArray subpredicates)

-- | @- displayableSubpredicatesOfPredicate:@
displayableSubpredicatesOfPredicate :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSPredicate predicate) => nsPredicateEditorRowTemplate -> predicate -> IO (Id NSArray)
displayableSubpredicatesOfPredicate nsPredicateEditorRowTemplate predicate =
  sendMessage nsPredicateEditorRowTemplate displayableSubpredicatesOfPredicateSelector (toNSPredicate predicate)

-- | @- initWithLeftExpressions:rightExpressions:modifier:operators:options:@
initWithLeftExpressions_rightExpressions_modifier_operators_options :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray leftExpressions, IsNSArray rightExpressions, IsNSArray operators) => nsPredicateEditorRowTemplate -> leftExpressions -> rightExpressions -> NSComparisonPredicateModifier -> operators -> CULong -> IO (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressions_modifier_operators_options nsPredicateEditorRowTemplate leftExpressions rightExpressions modifier operators options =
  sendOwnedMessage nsPredicateEditorRowTemplate initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector (toNSArray leftExpressions) (toNSArray rightExpressions) modifier (toNSArray operators) options

-- | @- initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:@
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray leftExpressions, IsNSArray operators) => nsPredicateEditorRowTemplate -> leftExpressions -> NSAttributeType -> NSComparisonPredicateModifier -> operators -> CULong -> IO (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_options nsPredicateEditorRowTemplate leftExpressions attributeType modifier operators options =
  sendOwnedMessage nsPredicateEditorRowTemplate initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector (toNSArray leftExpressions) attributeType modifier (toNSArray operators) options

-- | @- initWithCompoundTypes:@
initWithCompoundTypes :: (IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate, IsNSArray compoundTypes) => nsPredicateEditorRowTemplate -> compoundTypes -> IO (Id NSPredicateEditorRowTemplate)
initWithCompoundTypes nsPredicateEditorRowTemplate compoundTypes =
  sendOwnedMessage nsPredicateEditorRowTemplate initWithCompoundTypesSelector (toNSArray compoundTypes)

-- | @+ templatesWithAttributeKeyPaths:inEntityDescription:@
templatesWithAttributeKeyPaths_inEntityDescription :: (IsNSArray keyPaths, IsNSEntityDescription entityDescription) => keyPaths -> entityDescription -> IO (Id NSArray)
templatesWithAttributeKeyPaths_inEntityDescription keyPaths entityDescription =
  do
    cls' <- getRequiredClass "NSPredicateEditorRowTemplate"
    sendClassMessage cls' templatesWithAttributeKeyPaths_inEntityDescriptionSelector (toNSArray keyPaths) (toNSEntityDescription entityDescription)

-- | @- templateViews@
templateViews :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
templateViews nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate templateViewsSelector

-- | @- leftExpressions@
leftExpressions :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
leftExpressions nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate leftExpressionsSelector

-- | @- rightExpressions@
rightExpressions :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
rightExpressions nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate rightExpressionsSelector

-- | @- rightExpressionAttributeType@
rightExpressionAttributeType :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO NSAttributeType
rightExpressionAttributeType nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate rightExpressionAttributeTypeSelector

-- | @- modifier@
modifier :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO NSComparisonPredicateModifier
modifier nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate modifierSelector

-- | @- operators@
operators :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
operators nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate operatorsSelector

-- | @- options@
options :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO CULong
options nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate optionsSelector

-- | @- compoundTypes@
compoundTypes :: IsNSPredicateEditorRowTemplate nsPredicateEditorRowTemplate => nsPredicateEditorRowTemplate -> IO (Id NSArray)
compoundTypes nsPredicateEditorRowTemplate =
  sendMessage nsPredicateEditorRowTemplate compoundTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @matchForPredicate:@
matchForPredicateSelector :: Selector '[Id NSPredicate] CDouble
matchForPredicateSelector = mkSelector "matchForPredicate:"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector '[Id NSPredicate] ()
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @predicateWithSubpredicates:@
predicateWithSubpredicatesSelector :: Selector '[Id NSArray] (Id NSPredicate)
predicateWithSubpredicatesSelector = mkSelector "predicateWithSubpredicates:"

-- | @Selector@ for @displayableSubpredicatesOfPredicate:@
displayableSubpredicatesOfPredicateSelector :: Selector '[Id NSPredicate] (Id NSArray)
displayableSubpredicatesOfPredicateSelector = mkSelector "displayableSubpredicatesOfPredicate:"

-- | @Selector@ for @initWithLeftExpressions:rightExpressions:modifier:operators:options:@
initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector :: Selector '[Id NSArray, Id NSArray, NSComparisonPredicateModifier, Id NSArray, CULong] (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressions_modifier_operators_optionsSelector = mkSelector "initWithLeftExpressions:rightExpressions:modifier:operators:options:"

-- | @Selector@ for @initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:@
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector :: Selector '[Id NSArray, NSAttributeType, NSComparisonPredicateModifier, Id NSArray, CULong] (Id NSPredicateEditorRowTemplate)
initWithLeftExpressions_rightExpressionAttributeType_modifier_operators_optionsSelector = mkSelector "initWithLeftExpressions:rightExpressionAttributeType:modifier:operators:options:"

-- | @Selector@ for @initWithCompoundTypes:@
initWithCompoundTypesSelector :: Selector '[Id NSArray] (Id NSPredicateEditorRowTemplate)
initWithCompoundTypesSelector = mkSelector "initWithCompoundTypes:"

-- | @Selector@ for @templatesWithAttributeKeyPaths:inEntityDescription:@
templatesWithAttributeKeyPaths_inEntityDescriptionSelector :: Selector '[Id NSArray, Id NSEntityDescription] (Id NSArray)
templatesWithAttributeKeyPaths_inEntityDescriptionSelector = mkSelector "templatesWithAttributeKeyPaths:inEntityDescription:"

-- | @Selector@ for @templateViews@
templateViewsSelector :: Selector '[] (Id NSArray)
templateViewsSelector = mkSelector "templateViews"

-- | @Selector@ for @leftExpressions@
leftExpressionsSelector :: Selector '[] (Id NSArray)
leftExpressionsSelector = mkSelector "leftExpressions"

-- | @Selector@ for @rightExpressions@
rightExpressionsSelector :: Selector '[] (Id NSArray)
rightExpressionsSelector = mkSelector "rightExpressions"

-- | @Selector@ for @rightExpressionAttributeType@
rightExpressionAttributeTypeSelector :: Selector '[] NSAttributeType
rightExpressionAttributeTypeSelector = mkSelector "rightExpressionAttributeType"

-- | @Selector@ for @modifier@
modifierSelector :: Selector '[] NSComparisonPredicateModifier
modifierSelector = mkSelector "modifier"

-- | @Selector@ for @operators@
operatorsSelector :: Selector '[] (Id NSArray)
operatorsSelector = mkSelector "operators"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] CULong
optionsSelector = mkSelector "options"

-- | @Selector@ for @compoundTypes@
compoundTypesSelector :: Selector '[] (Id NSArray)
compoundTypesSelector = mkSelector "compoundTypes"

