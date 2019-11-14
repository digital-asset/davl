// Generated from DA/NonEmpty.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as daml from '../../../ledger/types';
import * as jtv from '@mojotech/json-type-validation';
import packageId from '../packageId';
import * as DA_Action from '../DA/Action';
import * as DA_Foldable from '../DA/Foldable';
import * as DA_Internal_Prelude from '../DA/Internal/Prelude';
import * as DA_Internal_Record from '../DA/Internal/Record';
import * as DA_Internal_Template from '../DA/Internal/Template';
import * as DA_Traversable from '../DA/Traversable';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_Control_Exception_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/Control/Exception/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_DA_Types from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/DA/Types';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Base from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Base';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Classes from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Classes';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Num from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Num';
import * as pkg1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d_GHC_Show from '../../1d739910b47df63ee080c1a1df6e5f983f9c5a0573fc0a7c2c20d7592b96cb8d/GHC/Show';

const moduleName = 'DA.NonEmpty';
const templateId = (entityName: string): daml.TemplateId => ({packageId, moduleName, entityName});

export type NonEmpty<a8wG> = {
  hd: a8wG;
  tl: a8wG[];
};
export const NonEmpty = <a8wG>(a8wG: daml.Serializable<a8wG>): daml.Serializable<NonEmpty<a8wG>> => ({
  decoder: () => jtv.object({
    hd: a8wG.decoder(),
    tl: daml.List(a8wG).decoder(),
  }),
});
