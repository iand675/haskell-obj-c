{-# LANGUAGE StrictData #-}

-- | Intermediate representation for Objective-C declarations.
--
-- These types capture the information extracted from clang's AST JSON
-- and serve as the input to the Haskell code generator.
module ObjC.CodeGen.IR
  ( -- * Declarations
    ObjCInterface(..)
  , ObjCCategory(..)
  , ObjCProtocolDecl(..)
  , ObjCMethod(..)
  , ObjCProperty(..)
  , Availability(..)

    -- * Types
  , ObjCType(..)
  , Nullability(..)
  , TypeQualifier(..)

    -- * Struct definitions
  , StructDef(..)

    -- * Enum definitions
  , EnumDef(..)
  , EnumConstant(..)

    -- * Merged class (interface + categories)
  , ObjCClass(..)

    -- * Class hierarchy graph
  , ClassHierarchy(..)
  , emptyHierarchy
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- Objective-C type representation
-- ---------------------------------------------------------------------------

-- | Nullability annotation for ObjC pointer types.
data Nullability
  = Nonnull     -- ^ @_Nonnull@ — pointer is never nil
  | Nullable    -- ^ @_Nullable@ — pointer may be nil
  | Unspecified -- ^ No annotation — treated as nullable (conservative)
  deriving (Eq, Ord, Show)

-- | C type qualifier (const, volatile).
data TypeQualifier
  = QConst     -- ^ @const@
  | QVolatile  -- ^ @volatile@
  deriving (Eq, Ord, Show)

-- | Representation of an Objective-C type, parsed from clang's @qualType@
-- and @desugaredQualType@ strings.
data ObjCType
  = ObjCId (Maybe Text) Nullability
    -- ^ @id@ or a specific class pointer (e.g., @NSString *@).
    -- The 'Maybe Text' is the class name, 'Nothing' for bare @id@.
  | ObjCInstancetype
    -- ^ @instancetype@ — return type that matches the receiver's type.
  | ObjCClassType (Maybe Text)
    -- ^ @Class@ or a specific metaclass pointer.
  | ObjCSEL
    -- ^ @SEL@ — a selector.
  | ObjCPrimitive Text Text
    -- ^ A C primitive type. First field is the @qualType@ (e.g., @NSInteger@),
    -- second is the @desugaredQualType@ (e.g., @long@).
  | ObjCPointer ObjCType
    -- ^ A pointer to another type (e.g., @NSError **@).
  | ObjCBlock ObjCType [ObjCType]
    -- ^ A block type: return type and parameter types.
  | ObjCGeneric Text [ObjCType] Nullability
    -- ^ A generic ObjC type (e.g., @NSArray\<NSString *\> *@).
    -- Class name, type arguments, and nullability of the outer pointer.
  | ObjCStruct Text
    -- ^ A C struct type passed by value (e.g., @NSRange@, @CGRect@).
    -- The 'Text' is the typedef name.
  | ObjCVoid
    -- ^ @void@.
  | ObjCBool
    -- ^ @BOOL@ / @_Bool@.
  | ObjCQualified TypeQualifier ObjCType
    -- ^ A C type qualifier wrapping an inner type.
    -- E.g., @const char *@ parses as @ObjCQualified QConst (ObjCPointer ...)@.
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Availability
-- ---------------------------------------------------------------------------

-- | Platform availability information for a declaration.
data Availability = Availability
  { availPlatform   :: Text
    -- ^ Platform name (e.g., @\"macos\"@, @\"ios\"@).
  , availIntroduced :: Maybe Text
    -- ^ Version when the API was introduced (e.g., @\"10.15\"@).
  , availDeprecated :: Maybe Text
    -- ^ Version when the API was deprecated, if any.
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Method declarations
-- ---------------------------------------------------------------------------

-- | An Objective-C method declaration.
data ObjCMethod = ObjCMethod
  { methodSelector    :: Text
    -- ^ The selector string (e.g., @\"initWithString:\"@).
  , methodReturnType  :: ObjCType
    -- ^ Return type.
  , methodParams      :: [(Text, ObjCType)]
    -- ^ Parameter names and types.
  , methodIsClass     :: Bool
    -- ^ 'True' for class methods (@+@), 'False' for instance methods (@-@).
  , methodIsImplicit  :: Bool
    -- ^ 'True' for compiler-synthesized methods (e.g., property accessors).
  , methodAvailability :: [Availability]
    -- ^ Platform availability annotations.
  , methodDoc         :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  , methodOriginFramework :: Maybe Text
    -- ^ The framework that declared this method. For interface methods
    -- this is the interface's framework; for category methods this is
    -- the category's framework (which may differ from the class's
    -- framework).
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Property declarations
-- ---------------------------------------------------------------------------

-- | An Objective-C property declaration.
data ObjCProperty = ObjCProperty
  { propName       :: Text
    -- ^ The property name.
  , propType       :: ObjCType
    -- ^ The property's type.
  , propReadonly   :: Bool
    -- ^ 'True' if the property is @readonly@.
  , propCopy       :: Bool
    -- ^ 'True' if the property uses @copy@ semantics.
  , propNonatomic  :: Bool
    -- ^ 'True' if the property is @nonatomic@.
  , propIsClass    :: Bool
    -- ^ 'True' if this is a class property (@\@property (class, ...)@).
  , propNullability :: Nullability
    -- ^ Nullability of the property (if it's a pointer type).
  , propDoc        :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  , propOriginFramework :: Maybe Text
    -- ^ The framework that declared this property. For interface
    -- properties this is the interface's framework; for category
    -- properties this is the category's framework.
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Interface / Category / Protocol declarations (raw from AST)
-- ---------------------------------------------------------------------------

-- | A raw @\@interface@ declaration from the clang AST.
data ObjCInterface = ObjCInterface
  { ifaceName       :: Text
    -- ^ Class name.
  , ifaceSuperclass :: Maybe Text
    -- ^ Superclass name ('Nothing' for root classes).
  , ifaceTypeParams :: [Text]
    -- ^ Lightweight generic type parameters.
  , ifaceProtocols  :: [Text]
    -- ^ Directly adopted protocol names.
  , ifaceMethods    :: [ObjCMethod]
    -- ^ Methods declared on the interface itself.
  , ifaceProperties :: [ObjCProperty]
    -- ^ Properties declared on the interface itself.
  , ifaceFramework  :: Maybe Text
    -- ^ Source framework (extracted from the header file path).
  , ifaceDoc        :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  } deriving (Eq, Show)

-- | A raw @\@interface Foo (CategoryName)@ declaration.
data ObjCCategory = ObjCCategory
  { catClassName    :: Text
    -- ^ The class this category extends.
  , catName         :: Maybe Text
    -- ^ Category name ('Nothing' for unnamed extensions).
  , catProtocols    :: [Text]
    -- ^ Protocol conformance declared in this category.
  , catMethods      :: [ObjCMethod]
    -- ^ Methods added by this category.
  , catProperties   :: [ObjCProperty]
    -- ^ Properties added by this category.
  , catFramework    :: Maybe Text
    -- ^ Source framework (extracted from the header file path).
  , catDoc          :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  } deriving (Eq, Show)

-- | A raw @\@protocol@ declaration.
data ObjCProtocolDecl = ObjCProtocolDecl
  { protoDeclName       :: Text
    -- ^ Protocol name.
  , protoDeclAdopted    :: [Text]
    -- ^ Protocols this protocol extends.
  , protoDeclRequired   :: [ObjCMethod]
    -- ^ Required methods.
  , protoDeclOptional   :: [ObjCMethod]
    -- ^ Optional methods.
  , protoDeclReqProps   :: [ObjCProperty]
    -- ^ Required properties.
  , protoDeclOptProps   :: [ObjCProperty]
    -- ^ Optional properties.
  , protoDeclFramework  :: Maybe Text
    -- ^ Source framework (extracted from the header file path).
  , protoDeclDoc        :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Struct definitions
-- ---------------------------------------------------------------------------

-- | A C struct definition extracted from the clang AST.
data StructDef = StructDef
  { structTypedefName :: Text
    -- ^ The typedef name (e.g., @\"NSRange\"@, @\"CGRect\"@).
  , structCName       :: Text
    -- ^ The underlying C struct name (e.g., @\"_NSRange\"@, @\"CGRect\"@).
  , structFields      :: [(Text, ObjCType)]
    -- ^ Field names and types, in declaration order.
  , structFramework   :: Maybe Text
    -- ^ Source framework (extracted from the header file path).
  , structDoc         :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Enum definitions
-- ---------------------------------------------------------------------------

-- | A single enum constant (name and integer value).
data EnumConstant = EnumConstant
  { ecName  :: Text
    -- ^ The constant name (e.g., @\"NSWindowStyleMaskTitled\"@).
  , ecValue :: Integer
    -- ^ The integer value.
  } deriving (Eq, Ord, Show)

-- | An Objective-C enum definition extracted from the clang AST.
data EnumDef = EnumDef
  { enumName               :: Text
    -- ^ The enum type name (e.g., @\"NSWindowStyleMask\"@).
  , enumUnderlyingQual     :: Text
    -- ^ The qualType of the underlying type (e.g., @\"NSUInteger\"@).
  , enumUnderlyingDesugared :: Text
    -- ^ The desugaredQualType of the underlying type (e.g., @\"unsigned long\"@).
  , enumIsOptions          :: Bool
    -- ^ 'True' for @NS_OPTIONS@ (bitmask) enums, 'False' for @NS_ENUM@.
  , enumConstants          :: [EnumConstant]
    -- ^ The enum constants, in declaration order.
  , enumFramework          :: Maybe Text
    -- ^ Source framework (extracted from the header file path).
  , enumDoc                :: Maybe Text
    -- ^ Documentation comment extracted from the header, if any.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Merged class (interface + all categories)
-- ---------------------------------------------------------------------------

-- | A fully merged ObjC class: the interface declaration combined with
-- all category extensions found in the AST.
data ObjCClass = ObjCClass
  { className      :: Text
    -- ^ Class name.
  , classSuperclass :: Maybe Text
    -- ^ Superclass name ('Nothing' for root classes like @NSObject@).
  , classTypeParams :: [Text]
    -- ^ Lightweight generic type parameters.
  , classProtocols  :: [Text]
    -- ^ Directly adopted protocol names.
  , classMethods    :: [ObjCMethod]
    -- ^ All methods (from interface + categories), non-implicit only.
  , classProperties :: [ObjCProperty]
    -- ^ All properties (from interface + categories).
  , classFramework  :: Maybe Text
    -- ^ Source framework (from the primary interface declaration).
  , classDoc        :: Maybe Text
    -- ^ Documentation comment from the primary interface, if any.
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Class hierarchy graph
-- ---------------------------------------------------------------------------

-- | The complete class hierarchy, built from the parsed AST.
--
-- This structure merges forward declarations, categories, and interface
-- bodies into a single 'ObjCClass' per class name, and computes the
-- transitive closure of both class ancestry and protocol conformance.
data ClassHierarchy = ClassHierarchy
  { hierarchyClasses    :: Map Text ObjCClass
    -- ^ Class name → merged class definition.
  , hierarchyProtocols  :: Map Text ObjCProtocolDecl
    -- ^ Protocol name → protocol definition.
  , hierarchyParentOf   :: Map Text Text
    -- ^ Child class → direct superclass.
  , hierarchyProtoConforms :: Map Text (Set Text)
    -- ^ Class → directly adopted protocols.
  , hierarchyTopoOrder  :: [Text]
    -- ^ Class names in topological order (roots first).
  , hierarchyAncestors  :: Map Text (Set Text)
    -- ^ Class → all transitive superclass names.
  , hierarchyAllProtos  :: Map Text (Set Text)
    -- ^ Class → all protocols (direct + inherited from superclasses).
  , hierarchyFrameworks :: Map Text Text
    -- ^ Class name → framework name (only for classes with known frameworks).
  , hierarchyStructs :: Map Text StructDef
    -- ^ Struct typedef name → struct definition.
  , hierarchyEnums :: Map Text EnumDef
    -- ^ Enum type name → enum definition.
  } deriving (Eq, Show)

-- | An empty hierarchy with no classes or protocols.
emptyHierarchy :: ClassHierarchy
emptyHierarchy = ClassHierarchy
  { hierarchyClasses       = Map.empty
  , hierarchyProtocols     = Map.empty
  , hierarchyParentOf      = Map.empty
  , hierarchyProtoConforms = Map.empty
  , hierarchyTopoOrder     = []
  , hierarchyAncestors     = Map.empty
  , hierarchyAllProtos     = Map.empty
  , hierarchyFrameworks    = Map.empty
  , hierarchyStructs      = Map.empty
  , hierarchyEnums        = Map.empty
  }
