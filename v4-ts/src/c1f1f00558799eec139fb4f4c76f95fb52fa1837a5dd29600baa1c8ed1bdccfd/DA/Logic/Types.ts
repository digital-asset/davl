// Generated from DA/Logic/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type Formula<a> = 
  |  { tag: 'Proposition'; value: a }
  |  { tag: 'Negation'; value: Formula<a> }
  |  { tag: 'Conjunction'; value: Formula<a>[] }
  |  { tag: 'Disjunction'; value: Formula<a>[] }
export const Formula = <a>(a: daml.Serializable<a>): daml.Serializable<Formula<a>> => ({
  decoder: () => jtv.oneOf<Formula<a>>(
    jtv.object({tag: jtv.constant('Proposition'), value: jtv.lazy(() => a.decoder())}),
    jtv.object({tag: jtv.constant('Negation'), value: jtv.lazy(() => Formula(a).decoder())}),
    jtv.object({tag: jtv.constant('Conjunction'), value: jtv.lazy(() => daml.List(Formula(a)).decoder())}),
    jtv.object({tag: jtv.constant('Disjunction'), value: jtv.lazy(() => daml.List(Formula(a)).decoder())}),
  ),
});
