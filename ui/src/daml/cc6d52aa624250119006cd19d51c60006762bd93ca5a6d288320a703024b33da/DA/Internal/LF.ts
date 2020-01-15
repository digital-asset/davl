// Generated from DA/Internal/LF.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type TemplateTypeRep = {
  getTemplateTypeRep: string;
}
export const TemplateTypeRep: daml.Serializable<TemplateTypeRep> = ({
  decoder: () => jtv.object({
    getTemplateTypeRep: daml.Text.decoder(),
  }),
})

export type AnyChoice = {
  getAnyChoice: {};
}
export const AnyChoice: daml.Serializable<AnyChoice> = ({
  decoder: () => jtv.object({
    getAnyChoice: daml.Unit.decoder(),
  }),
})

export type AnyTemplate = {
  getAnyTemplate: {};
}
export const AnyTemplate: daml.Serializable<AnyTemplate> = ({
  decoder: () => jtv.object({
    getAnyTemplate: daml.Unit.decoder(),
  }),
})
