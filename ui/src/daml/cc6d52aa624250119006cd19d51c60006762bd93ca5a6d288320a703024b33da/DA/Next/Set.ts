// Generated from DA/Next/Set.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';
import * as DA_Internal_Prelude from '../../DA/Internal/Prelude';
import * as DA_Internal_Record from '../../DA/Internal/Record';
import * as DA_Internal_Template from '../../DA/Internal/Template';
import * as DA_Next_Map from '../../DA/Next/Map';
import * as DA_TextMap from '../../DA/TextMap';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Next.Set';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type Set<a9z2> = {
  textMap: { [key: string]: {} };
};
export const Set = <a9z2>(a9z2: daml.Serializable<a9z2>): daml.Serializable<Set<a9z2>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(daml.Unit).decoder(),
  }),
});
