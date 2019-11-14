// Generated from DA/Next/Map.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../../packageId';
import * as DA_Foldable from '../../DA/Foldable';
import * as DA_Internal_LF from '../../DA/Internal/LF';
import * as DA_Internal_Prelude from '../../DA/Internal/Prelude';
import * as DA_Internal_Record from '../../DA/Internal/Record';
import * as DA_Optional from '../../DA/Optional';
import * as DA_Text from '../../DA/Text';
import * as DA_TextMap from '../../DA/TextMap';
import * as DA_Traversable from '../../DA/Traversable';
import * as DA_Tuple from '../../DA/Tuple';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Num from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Num';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Types from '../../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Types';

const moduleName = 'DA.Next.Map';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});


export type Map<a8Om, a8On> = {
  textMap: { [key: string]: a8On };
};
export const Map = <a8Om, a8On>(a8Om: daml.Serializable<a8Om>, a8On: daml.Serializable<a8On>): daml.Serializable<Map<a8Om, a8On>> => ({
  decoder: () => jtv.object({
    textMap: daml.TextMap(a8On).decoder(),
  }),
});
