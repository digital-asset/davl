// Generated from DA/Upgrade.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Generics from '../DA/Generics';
import * as DA_Internal_Assert from '../DA/Internal/Assert';
import * as DA_Internal_LF from '../DA/Internal/LF';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as DA_Internal_Record from '../DA/Internal/Record';
import * as DA_Internal_Template from '../DA/Internal/Template';
import * as DA_Next_Map from '../DA/Next/Map';
import * as DA_Next_Set from '../DA/Next/Set';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_Control_Exception_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/Control/Exception/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';

const moduleName = 'DA.Upgrade';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type MetaEquiv<a9Ou, a9Ov> = {
};
export const MetaEquiv = <a9Ou, a9Ov>(a9Ou: daml.Serializable<a9Ou>, a9Ov: daml.Serializable<a9Ov>): daml.Serializable<MetaEquiv<a9Ou, a9Ov>> => ({
  decoder: () => jtv.object({
  }),
});

export type DoRollback<a9Pc> = {
  inC: daml.ContractId<a9Pc>;
  sigs: daml.Party[];
};
export const DoRollback = <a9Pc>(a9Pc: daml.Serializable<a9Pc>): daml.Serializable<DoRollback<a9Pc>> => ({
  decoder: () => jtv.object({
    inC: daml.ContractId(a9Pc).decoder(),
    sigs: daml.List(daml.Party).decoder(),
  }),
});

export type Rollback<a9Pd, a9Pe> = {
  op: daml.Party;
};
export const Rollback = <a9Pd, a9Pe>(a9Pd: daml.Serializable<a9Pd>, a9Pe: daml.Serializable<a9Pe>): daml.Serializable<Rollback<a9Pd, a9Pe>> => ({
  decoder: () => jtv.object({
    op: daml.Party.decoder(),
  }),
});

export type DoUpgrade<a9PO> = {
  inC: daml.ContractId<a9PO>;
  sigs: daml.Party[];
};
export const DoUpgrade = <a9PO>(a9PO: daml.Serializable<a9PO>): daml.Serializable<DoUpgrade<a9PO>> => ({
  decoder: () => jtv.object({
    inC: daml.ContractId(a9PO).decoder(),
    sigs: daml.List(daml.Party).decoder(),
  }),
});

export type Upgrade<a9PP, a9PQ> = {
  op: daml.Party;
};
export const Upgrade = <a9PP, a9PQ>(a9PP: daml.Serializable<a9PP>, a9PQ: daml.Serializable<a9PQ>): daml.Serializable<Upgrade<a9PP, a9PQ>> => ({
  decoder: () => jtv.object({
    op: daml.Party.decoder(),
  }),
});
