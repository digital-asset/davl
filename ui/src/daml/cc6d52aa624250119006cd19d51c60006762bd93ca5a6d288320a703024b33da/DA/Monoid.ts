// Generated from DA/Monoid.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as DA_Internal_Record from '../DA/Internal/Record';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Num from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Num';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Monoid';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Product<a4Dj> = {
  unpack: a4Dj;
};
export const Product = <a4Dj>(a4Dj: daml.Serializable<a4Dj>): daml.Serializable<Product<a4Dj>> => ({
  decoder: () => jtv.object({
    unpack: a4Dj.decoder(),
  }),
});

export type Sum<a4Dk> = {
  unpack: a4Dk;
};
export const Sum = <a4Dk>(a4Dk: daml.Serializable<a4Dk>): daml.Serializable<Sum<a4Dk>> => ({
  decoder: () => jtv.object({
    unpack: a4Dk.decoder(),
  }),
});


export type Any = {
  getAny: boolean;
};
export const Any: daml.Serializable<Any> = ({
  decoder: () => jtv.object({
    getAny: daml.Bool.decoder(),
  }),
});

export type All = {
  getAll: boolean;
};
export const All: daml.Serializable<All> = ({
  decoder: () => jtv.object({
    getAll: daml.Bool.decoder(),
  }),
});
