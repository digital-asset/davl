// Generated from DA/Internal/Template.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';

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
