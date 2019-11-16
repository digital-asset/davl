// Generated from DA/Internal/LF.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '@digitalasset/daml-json-types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';
import * as DA_Internal_Prelude from '../../DA/Internal/Prelude';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Internal.LF';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type TemplateTypeRep = {
  getTemplateTypeRep: string;
};
export const TemplateTypeRep: daml.Serializable<TemplateTypeRep> = ({
  decoder: () => jtv.object({
    getTemplateTypeRep: daml.Text.decoder(),
  }),
});

export type AnyChoice = {
  getAnyChoice: {};
};
export const AnyChoice: daml.Serializable<AnyChoice> = ({
  decoder: () => jtv.object({
    getAnyChoice: daml.Unit.decoder(),
  }),
});

export type AnyTemplate = {
  getAnyTemplate: {};
};
export const AnyTemplate: daml.Serializable<AnyTemplate> = ({
  decoder: () => jtv.object({
    getAnyTemplate: daml.Unit.decoder(),
  }),
});
