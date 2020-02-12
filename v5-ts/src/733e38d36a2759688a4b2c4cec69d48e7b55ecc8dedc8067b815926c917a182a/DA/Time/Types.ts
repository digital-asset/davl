// Generated from DA/Time/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export type RelTime = {
  microseconds: daml.Int;
}
export const RelTime: daml.Serializable<RelTime> = ({
  decoder: () => jtv.object({
    microseconds: daml.Int.decoder(),
  }),
})
