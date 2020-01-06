// Generated from DA/Monoid.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Product<a4Dj> = {
  unpack: a4Dj;
};
export const Product = <a4Dj>(a4Dj: daml.Serializable<a4Dj>): daml.Serializable<Product<a4Dj>> => ({
  decoder: () => jtv.object({
    unpack: a4Dj.decoder(),
  }),
});

export type Sum<a4Dk> = {
  unpack: a4Dk;
};
export const Sum = <a4Dk>(a4Dk: daml.Serializable<a4Dk>): daml.Serializable<Sum<a4Dk>> => ({
  decoder: () => jtv.object({
    unpack: a4Dk.decoder(),
  }),
});

export type Any = {
  getAny: boolean;
};
export const Any: daml.Serializable<Any> = ({
  decoder: () => jtv.object({
    getAny: daml.Bool.decoder(),
  }),
});

export type All = {
  getAll: boolean;
};
export const All: daml.Serializable<All> = ({
  decoder: () => jtv.object({
    getAll: daml.Bool.decoder(),
  }),
});
