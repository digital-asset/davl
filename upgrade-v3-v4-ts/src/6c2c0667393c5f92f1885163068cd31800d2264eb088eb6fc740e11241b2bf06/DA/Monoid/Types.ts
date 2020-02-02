// Generated from DA/Monoid/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type All = {
  getAll: boolean;
}
export const All: daml.Serializable<All> = ({
  decoder: () => jtv.object({
    getAll: daml.Bool.decoder(),
  }),
})

export type Any = {
  getAny: boolean;
}
export const Any: daml.Serializable<Any> = ({
  decoder: () => jtv.object({
    getAny: daml.Bool.decoder(),
  }),
})

export type Sum<a> = {
  unpack: a;
}
export const Sum = <a>(a: daml.Serializable<a>): daml.Serializable<Sum<a>> => ({
  decoder: () => jtv.object({
    unpack: a.decoder(),
  }),
})

export type Product<a> = {
  unpack: a;
}
export const Product = <a>(a: daml.Serializable<a>): daml.Serializable<Product<a>> => ({
  decoder: () => jtv.object({
    unpack: a.decoder(),
  }),
})
