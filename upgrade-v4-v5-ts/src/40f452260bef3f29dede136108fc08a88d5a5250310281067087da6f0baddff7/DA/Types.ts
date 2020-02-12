// Generated from DA/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Either<a, b> = 
  |  { tag: 'Left'; value: a }
  |  { tag: 'Right'; value: b }
export const Either = <a, b>(a: daml.Serializable<a>, b: daml.Serializable<b>): daml.Serializable<Either<a, b>> => ({
  decoder: () => jtv.oneOf<Either<a, b>>(
    jtv.object({tag: jtv.constant('Left'), value: jtv.lazy(() => a.decoder())}),
    jtv.object({tag: jtv.constant('Right'), value: jtv.lazy(() => b.decoder())}),
  ),
});

export type Tuple2<t1, t2> = {
  _1: t1;
  _2: t2;
}
export const Tuple2 = <t1, t2>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>): daml.Serializable<Tuple2<t1, t2>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
  }),
})

export type Tuple3<t1, t2, t3> = {
  _1: t1;
  _2: t2;
  _3: t3;
}
export const Tuple3 = <t1, t2, t3>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>): daml.Serializable<Tuple3<t1, t2, t3>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
  }),
})

export type Tuple4<t1, t2, t3, t4> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
}
export const Tuple4 = <t1, t2, t3, t4>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>): daml.Serializable<Tuple4<t1, t2, t3, t4>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
  }),
})

export type Tuple5<t1, t2, t3, t4, t5> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
}
export const Tuple5 = <t1, t2, t3, t4, t5>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>): daml.Serializable<Tuple5<t1, t2, t3, t4, t5>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
  }),
})

export type Tuple6<t1, t2, t3, t4, t5, t6> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
}
export const Tuple6 = <t1, t2, t3, t4, t5, t6>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>): daml.Serializable<Tuple6<t1, t2, t3, t4, t5, t6>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
  }),
})

export type Tuple7<t1, t2, t3, t4, t5, t6, t7> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
}
export const Tuple7 = <t1, t2, t3, t4, t5, t6, t7>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>): daml.Serializable<Tuple7<t1, t2, t3, t4, t5, t6, t7>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
  }),
})

export type Tuple8<t1, t2, t3, t4, t5, t6, t7, t8> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
}
export const Tuple8 = <t1, t2, t3, t4, t5, t6, t7, t8>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>): daml.Serializable<Tuple8<t1, t2, t3, t4, t5, t6, t7, t8>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
  }),
})

export type Tuple9<t1, t2, t3, t4, t5, t6, t7, t8, t9> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
}
export const Tuple9 = <t1, t2, t3, t4, t5, t6, t7, t8, t9>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>): daml.Serializable<Tuple9<t1, t2, t3, t4, t5, t6, t7, t8, t9>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
  }),
})

export type Tuple10<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
}
export const Tuple10 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>): daml.Serializable<Tuple10<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
  }),
})

export type Tuple11<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
}
export const Tuple11 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>): daml.Serializable<Tuple11<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
  }),
})

export type Tuple12<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
}
export const Tuple12 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>): daml.Serializable<Tuple12<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
  }),
})

export type Tuple13<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
}
export const Tuple13 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>): daml.Serializable<Tuple13<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
  }),
})

export type Tuple14<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
}
export const Tuple14 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>): daml.Serializable<Tuple14<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
  }),
})

export type Tuple15<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
}
export const Tuple15 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>): daml.Serializable<Tuple15<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
  }),
})

export type Tuple16<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
  _16: t16;
}
export const Tuple16 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>, t16: daml.Serializable<t16>): daml.Serializable<Tuple16<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
    _16: t16.decoder(),
  }),
})

export type Tuple17<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
  _16: t16;
  _17: t17;
}
export const Tuple17 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>, t16: daml.Serializable<t16>, t17: daml.Serializable<t17>): daml.Serializable<Tuple17<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
    _16: t16.decoder(),
    _17: t17.decoder(),
  }),
})

export type Tuple18<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
  _16: t16;
  _17: t17;
  _18: t18;
}
export const Tuple18 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>, t16: daml.Serializable<t16>, t17: daml.Serializable<t17>, t18: daml.Serializable<t18>): daml.Serializable<Tuple18<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
    _16: t16.decoder(),
    _17: t17.decoder(),
    _18: t18.decoder(),
  }),
})

export type Tuple19<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
  _16: t16;
  _17: t17;
  _18: t18;
  _19: t19;
}
export const Tuple19 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>, t16: daml.Serializable<t16>, t17: daml.Serializable<t17>, t18: daml.Serializable<t18>, t19: daml.Serializable<t19>): daml.Serializable<Tuple19<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
    _16: t16.decoder(),
    _17: t17.decoder(),
    _18: t18.decoder(),
    _19: t19.decoder(),
  }),
})

export type Tuple20<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20> = {
  _1: t1;
  _2: t2;
  _3: t3;
  _4: t4;
  _5: t5;
  _6: t6;
  _7: t7;
  _8: t8;
  _9: t9;
  _10: t10;
  _11: t11;
  _12: t12;
  _13: t13;
  _14: t14;
  _15: t15;
  _16: t16;
  _17: t17;
  _18: t18;
  _19: t19;
  _20: t20;
}
export const Tuple20 = <t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20>(t1: daml.Serializable<t1>, t2: daml.Serializable<t2>, t3: daml.Serializable<t3>, t4: daml.Serializable<t4>, t5: daml.Serializable<t5>, t6: daml.Serializable<t6>, t7: daml.Serializable<t7>, t8: daml.Serializable<t8>, t9: daml.Serializable<t9>, t10: daml.Serializable<t10>, t11: daml.Serializable<t11>, t12: daml.Serializable<t12>, t13: daml.Serializable<t13>, t14: daml.Serializable<t14>, t15: daml.Serializable<t15>, t16: daml.Serializable<t16>, t17: daml.Serializable<t17>, t18: daml.Serializable<t18>, t19: daml.Serializable<t19>, t20: daml.Serializable<t20>): daml.Serializable<Tuple20<t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20>> => ({
  decoder: () => jtv.object({
    _1: t1.decoder(),
    _2: t2.decoder(),
    _3: t3.decoder(),
    _4: t4.decoder(),
    _5: t5.decoder(),
    _6: t6.decoder(),
    _7: t7.decoder(),
    _8: t8.decoder(),
    _9: t9.decoder(),
    _10: t10.decoder(),
    _11: t11.decoder(),
    _12: t12.decoder(),
    _13: t13.decoder(),
    _14: t14.decoder(),
    _15: t15.decoder(),
    _16: t16.decoder(),
    _17: t17.decoder(),
    _18: t18.decoder(),
    _19: t19.decoder(),
    _20: t20.decoder(),
  }),
})
