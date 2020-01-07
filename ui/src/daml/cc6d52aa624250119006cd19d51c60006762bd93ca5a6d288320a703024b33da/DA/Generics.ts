// Generated from DA/Generics.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type MetaSel0 = {
  mbRecordName: ({} | null);
  sourceUnpackedness: SourceUnpackedness;
  sourceStrictness: SourceStrictness;
};
export const MetaSel0: daml.Serializable<MetaSel0> = ({
  decoder: () => jtv.object({
    mbRecordName: daml.Optional(daml.Unit).decoder(),
    sourceUnpackedness: SourceUnpackedness.decoder(),
    sourceStrictness: SourceStrictness.decoder(),
  }),
});

export type MetaData0 = {
  name: {};
  module_: {};
  package: {};
  isNewType: boolean;
};
export const MetaData0: daml.Serializable<MetaData0> = ({
  decoder: () => jtv.object({
    name: daml.Unit.decoder(),
    module_: daml.Unit.decoder(),
    package: daml.Unit.decoder(),
    isNewType: daml.Bool.decoder(),
  }),
});

export enum DecidedStrictness{
  DecidedLazy = 'DecidedLazy',
  DecidedStrict = 'DecidedStrict',
  DecidedUnpack = 'DecidedUnpack',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<DecidedStrictness>(DecidedStrictness)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace DecidedStrictness{
  export const decoder =
  () => jtv.oneOf(
    jtv.constant(DecidedStrictness.DecidedLazy),
    jtv.constant(DecidedStrictness.DecidedStrict),
    jtv.constant(DecidedStrictness.DecidedUnpack),
  )
}

export enum SourceStrictness{
  NoSourceStrictness = 'NoSourceStrictness',
  SourceLazy = 'SourceLazy',
  SourceStrict = 'SourceStrict',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<SourceStrictness>(SourceStrictness)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace SourceStrictness{
  export const decoder =
  () => jtv.oneOf(
    jtv.constant(SourceStrictness.NoSourceStrictness),
    jtv.constant(SourceStrictness.SourceLazy),
    jtv.constant(SourceStrictness.SourceStrict),
  )
}

export enum SourceUnpackedness{
  NoSourceUnpackedness = 'NoSourceUnpackedness',
  SourceNoUnpack = 'SourceNoUnpack',
  SourceUnpack = 'SourceUnpack',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<SourceUnpackedness>(SourceUnpackedness)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace SourceUnpackedness{
  export const decoder =
  () => jtv.oneOf(
    jtv.constant(SourceUnpackedness.NoSourceUnpackedness),
    jtv.constant(SourceUnpackedness.SourceNoUnpack),
    jtv.constant(SourceUnpackedness.SourceUnpack),
  )
}

export enum Associativity{
  LeftAssociative = 'LeftAssociative',
  RightAssociative = 'RightAssociative',
  NotAssociative = 'NotAssociative',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<Associativity>(Associativity)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace Associativity{
  export const decoder =
  () => jtv.oneOf(
    jtv.constant(Associativity.LeftAssociative),
    jtv.constant(Associativity.RightAssociative),
    jtv.constant(Associativity.NotAssociative),
  )
}

export type Infix0 = {
  associativity: Associativity;
  fixity: daml.Int;
};
export const Infix0: daml.Serializable<Infix0> = ({
  decoder: () => jtv.object({
    associativity: Associativity.decoder(),
    fixity: daml.Int.decoder(),
  }),
});

export type Fixity = unknown;
export const Fixity: daml.Serializable<Fixity> = ({
  decoder: jtv.unknownJson,
});

export type K1<a22z, a22A, a22B> = {
  unK1: a22A;
};
export const K1 = <a22z, a22A, a22B>(a22z: daml.Serializable<a22z>, a22A: daml.Serializable<a22A>, a22B: daml.Serializable<a22B>): daml.Serializable<K1<a22z, a22A, a22B>> => ({
  decoder: () => jtv.object({
    unK1: a22A.decoder(),
  }),
});

export type Par1<a22E> = {
  unPar1: a22E;
};
export const Par1 = <a22E>(a22E: daml.Serializable<a22E>): daml.Serializable<Par1<a22E>> => ({
  decoder: () => jtv.object({
    unPar1: a22E.decoder(),
  }),
});

export type U1<a22F> = {
};
export const U1 = <a22F>(a22F: daml.Serializable<a22F>): daml.Serializable<U1<a22F>> => ({
  decoder: () => jtv.object({
  }),
});
