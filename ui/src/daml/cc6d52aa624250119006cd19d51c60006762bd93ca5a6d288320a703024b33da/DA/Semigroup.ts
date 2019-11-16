// Generated from DA/Semigroup.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Semigroup';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Max<a4ip> = {
  unpack: a4ip;
};
export const Max = <a4ip>(a4ip: daml.Serializable<a4ip>): daml.Serializable<Max<a4ip>> => ({
  decoder: () => jtv.object({
    unpack: a4ip.decoder(),
  }),
});

export type Min<a4iq> = {
  unpack: a4iq;
};
export const Min = <a4iq>(a4iq: daml.Serializable<a4iq>): daml.Serializable<Min<a4iq>> => ({
  decoder: () => jtv.object({
    unpack: a4iq.decoder(),
  }),
});
