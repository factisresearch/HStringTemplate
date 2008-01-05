-----------------------------------------------------------------------------
-- |
-- Module      :  Text.StringTemplate.Base
-- Copyright   :  (c) Sterling Clover 2008
-- License     :  BSD 3 clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  0.1
--
-- A StringTemplate is a String with \"holes\" in it.
-- This is a port of the Java StringTemplate library written by Terrence Parr.
-- (<http://www.stringtemplate.org>).
--
-- This library implements the basic 3.0 grammar, lacking Groups,
-- Regions, and Interfaces.
-- Additionally, it does not yet implement indentation and wrapping.
-- The scheme is to add these as an additional layer at some point.
--
-- This library does extend the current StringTemplate grammar by allowing the
-- application of alternating attributes to anonymous
-- as well as regular templates, including therefore sets of alternating
-- attributes.
--
-- Basic instances are provided of the StringTemplateShows class.
-- Any type deriving this class can be passed automatically as a StringTemplate
-- attribute. Additional bindings for HAppS Data, JSON, etc. are anticipated.
-----------------------------------------------------------------------------
module Text.StringTemplate
    (StringTemplate, toString, newSTMP, newAngleSTMP, 
     StringTemplateShows(..), ToSElem(..), STGen, setAttribute,
     groupStringTemplates, addSuperGroup, addSubGroup, mergeSTGroups,
     stringTemplateFileGroup, cacheSTGroup
    ) where
    import Text.StringTemplate.Base
    import Text.StringTemplate.Instances