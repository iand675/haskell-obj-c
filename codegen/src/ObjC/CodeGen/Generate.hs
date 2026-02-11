{-# LANGUAGE OverloadedStrings #-}

-- | Haskell source code generation from the class hierarchy IR.
--
-- This module is a facade that re-exports the public API from the
-- sub-modules under @ObjC.CodeGen.Generate.*@.
--
-- Generates one Haskell package per Objective-C framework, each containing:
--
-- * One module per class with phantom type tags, type class hierarchy,
--   and typed method wrappers
-- * A framework-level re-export module
-- * A @package.yaml@ with correct cross-framework dependencies
module ObjC.CodeGen.Generate
  ( -- * Types
    GeneratedModule(..)
  , GeneratedPackage(..)
  , SkippedBinding(..)
    -- * Top-level generation
  , generateFrameworkPackages
    -- * Type closure (used by tests / tooling)
  , computeTypeClosure
  , classReferencedTypes
  ) where

import ObjC.CodeGen.Generate.Types
  ( GeneratedModule(..)
  , GeneratedPackage(..)
  , SkippedBinding(..)
  )
import ObjC.CodeGen.Generate.Package
  ( generateFrameworkPackages
  , computeTypeClosure
  , classReferencedTypes
  )
