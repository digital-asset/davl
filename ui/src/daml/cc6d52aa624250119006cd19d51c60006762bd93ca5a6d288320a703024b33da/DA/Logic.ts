// Generated from DA/Logic.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@digitalasset/daml-json-types';

export type Formula<a6PY> = 
  |  { tag: 'Proposition'; value: a6PY }
  |  { tag: 'Negation'; value: Formula<a6PY> }
  |  { tag: 'Conjunction'; value: Formula<a6PY>[] }
  |  { tag: 'Disjunction'; value: Formula<a6PY>[] }
export const Formula = <a6PY>(a6PY: daml.Serializable<a6PY>): daml.Serializable<Formula<a6PY>> => ({
  decoder: () => jtv.oneOf(
    jtv.object<Formula<a6PY>>({tag: jtv.constant('Proposition'), value: jtv.lazy(() => a6PY.decoder())}),
    jtv.object<Formula<a6PY>>({tag: jtv.constant('Negation'), value: jtv.lazy(() => Formula(a6PY).decoder())}),
    jtv.object<Formula<a6PY>>({tag: jtv.constant('Conjunction'), value: jtv.lazy(() => daml.List(Formula(a6PY)).decoder())}),
    jtv.object<Formula<a6PY>>({tag: jtv.constant('Disjunction'), value: jtv.lazy(() => daml.List(Formula(a6PY)).decoder())}),
  )
});
