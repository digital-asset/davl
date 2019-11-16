// Generated from DA/Generics.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_Control_Exception_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/Control/Exception/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Enum from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Enum';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Err from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Err';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Num from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Num';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Prim from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Prim';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Generics';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type MetaSel0 = {
  mbRecordName: ({} | null);
  sourceUnpackedness: SourceUnpackedness;
  sourceStrictness: SourceStrictness;
};
export const MetaSel0: daml.Serializable<MetaSel0> = ({
  decoder: () => jtv.object({
    mbRecordName: daml.Optional(daml.Unit).decoder(),
    sourceUnpackedness: SourceUnpackedness.decoder(),
    sourceStrictness: SourceStrictness.decoder(),
  }),
});

export type MetaData0 = {
  name: {};
  module_: {};
  package: {};
  isNewType: boolean;
};
export const MetaData0: daml.Serializable<MetaData0> = ({
  decoder: () => jtv.object({
    name: daml.Unit.decoder(),
    module_: daml.Unit.decoder(),
    package: daml.Unit.decoder(),
    isNewType: daml.Bool.decoder(),
  }),
});

export type DecidedStrictness = unknown;
export const DecidedStrictness: daml.Serializable<DecidedStrictness> = ({
  decoder: jtv.unknownJson,
});

export type SourceStrictness = unknown;
export const SourceStrictness: daml.Serializable<SourceStrictness> = ({
  decoder: jtv.unknownJson,
});

export type SourceUnpackedness = unknown;
export const SourceUnpackedness: daml.Serializable<SourceUnpackedness> = ({
  decoder: jtv.unknownJson,
});

export type Associativity = unknown;
export const Associativity: daml.Serializable<Associativity> = ({
  decoder: jtv.unknownJson,
});

export type Infix0 = {
  associativity: Associativity;
  fixity: daml.Int;
};
export const Infix0: daml.Serializable<Infix0> = ({
  decoder: () => jtv.object({
    associativity: Associativity.decoder(),
    fixity: daml.Int.decoder(),
  }),
});

export type Fixity = unknown;
export const Fixity: daml.Serializable<Fixity> = ({
  decoder: jtv.unknownJson,
});

export type K1<a22z, a22A, a22B> = {
  unK1: a22A;
};
export const K1 = <a22z, a22A, a22B>(a22z: daml.Serializable<a22z>, a22A: daml.Serializable<a22A>, a22B: daml.Serializable<a22B>): daml.Serializable<K1<a22z, a22A, a22B>> => ({
  decoder: () => jtv.object({
    unK1: a22A.decoder(),
  }),
});

export type Par1<a22E> = {
  unPar1: a22E;
};
export const Par1 = <a22E>(a22E: daml.Serializable<a22E>): daml.Serializable<Par1<a22E>> => ({
  decoder: () => jtv.object({
    unPar1: a22E.decoder(),
  }),
});

export type U1<a22F> = {
};
export const U1 = <a22F>(a22F: daml.Serializable<a22F>): daml.Serializable<U1<a22F>> => ({
  decoder: () => jtv.object({
  }),
});
