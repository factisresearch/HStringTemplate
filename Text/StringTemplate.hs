-----------------------------------------------------------------------------
-- |
-- Module      :  Text.StringTemplate.Base
-- Copyright   :  (c) Sterling Clover 2008
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A StringTemplate is a String with \"holes\" in it.
-- This is a port of the Java StringTemplate library written by Terrence Parr.
-- (<http://www.stringtemplate.org>).
--
-- This library implements the basic 3.0 grammar, lacking group files
-- (though not groups themselves), Regions, and Interfaces.
-- The goal is not to blindly copy the StringTemplate API, but rather to
-- take its central ideas and implement them in a Haskellish manner.
-- Indentation and wrapping, for example, are implemented through the
-- HughesPJ Pretty Printing library. Calling toPPDoc on a StringTemplate
-- yields a Doc with appropriate paragraph-fill wrapping that can be
-- rendered in the usual fashion.
--
-- This library extends the current StringTemplate grammar by allowing the
-- application of alternating attributes to anonymous
-- as well as regular templates, including therefore sets of alternating
-- attributes.
--
-- Basic instances are provided of the StringTemplateShows class.
-- Any type deriving this class can be passed automatically as a StringTemplate
-- attribute. Additional bindings for HAppS Data, JSON, etc. are anticipated.
-- When defining an instance of the StringTemplateShows you may optionally
-- define a stringTemplateFormattedShows method that parses a format string.
-----------------------------------------------------------------------------

module Text.StringTemplate (
  -- * Types
  StringTemplate, STGroup,
  -- * Classes
  StringTemplateShows(..), ToSElem(..), Stringable(..),
  -- * Creation
  newSTMP, newAngleSTMP, getStringTemplate,
  -- * Display
  toString, toPPDoc, render,
  -- * Modification
  setAttribute, setManyAttrib, optInsertTmpl, optInsertGroup,
  setEncoder, setEncoderGroup,
  -- * Groups
  groupStringTemplates, addSuperGroup, addSubGroup,
  mergeSTGroups, directoryGroup, unsafeVolatileDirectoryGroup
  ) where
import Text.StringTemplate.Base
import Text.StringTemplate.Group
import Text.StringTemplate.Instances()
