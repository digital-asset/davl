// Generated from DA/Validation.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as DA_NonEmpty from '../DA/NonEmpty';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_Control_Exception_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/Control/Exception/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';

const moduleName = 'DA.Validation';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Validation<a9s4, a9s5> = unknown;
export const Validation = <a9s4, a9s5>(a9s4: daml.Serializable<a9s4>, a9s5: daml.Serializable<a9s5>): daml.Serializable<Validation<a9s4, a9s5>> => ({
  decoder: jtv.unknownJson,
});
