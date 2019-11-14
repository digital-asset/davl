// Generated from DA/Internal/Template.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';
import * as DA_Internal_LF from '../../DA/Internal/LF';
import * as DA_Internal_Prelude from '../../DA/Internal/Prelude';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';

const moduleName = 'DA.Internal.Template';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});


export type HasKey<a3AC> = {
};
export const HasKey = <a3AC>(a3AC: daml.Serializable<a3AC>): daml.Serializable<HasKey<a3AC>> => ({
  decoder: () => jtv.object({
  }),
});

export type Archive = {
};
export const Archive: daml.Serializable<Archive> = ({
  decoder: () => jtv.object({
  }),
});

export type PostConsuming<a3AD> = {
};
export const PostConsuming = <a3AD>(a3AD: daml.Serializable<a3AD>): daml.Serializable<PostConsuming<a3AD>> => ({
  decoder: () => jtv.object({
  }),
});

export type PreConsuming<a3AE> = {
};
export const PreConsuming = <a3AE>(a3AE: daml.Serializable<a3AE>): daml.Serializable<PreConsuming<a3AE>> => ({
  decoder: () => jtv.object({
  }),
});

export type NonConsuming<a3AF> = {
};
export const NonConsuming = <a3AF>(a3AF: daml.Serializable<a3AF>): daml.Serializable<NonConsuming<a3AF>> => ({
  decoder: () => jtv.object({
  }),
});



