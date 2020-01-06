// Generated from DA/Upgrade.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

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
