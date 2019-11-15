// Generated from DA/Logic.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Either from '../DA/Either';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Err from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Err';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Prim from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Prim';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Logic';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Formula<a6PY> = unknown;
export const Formula = <a6PY>(a6PY: daml.Serializable<a6PY>): daml.Serializable<Formula<a6PY>> => ({
  decoder: jtv.unknownJson,
});
