// Generated from DA/Date/Types.daml
/* eslint-disable @typescript-eslint/camelcase */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as jtv from '@mojotech/json-type-validation';
import * as daml from '@daml/types';

export enum DayOfWeek {
  Monday = 'Monday',
  Tuesday = 'Tuesday',
  Wednesday = 'Wednesday',
  Thursday = 'Thursday',
  Friday = 'Friday',
  Saturday = 'Saturday',
  Sunday = 'Sunday',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<DayOfWeek>(DayOfWeek)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace DayOfWeek {
  export const decoder = () => jtv.oneOf<DayOfWeek>(
    jtv.constant(DayOfWeek.Monday),
    jtv.constant(DayOfWeek.Tuesday),
    jtv.constant(DayOfWeek.Wednesday),
    jtv.constant(DayOfWeek.Thursday),
    jtv.constant(DayOfWeek.Friday),
    jtv.constant(DayOfWeek.Saturday),
    jtv.constant(DayOfWeek.Sunday),
  );
}

export enum Month {
  Jan = 'Jan',
  Feb = 'Feb',
  Mar = 'Mar',
  Apr = 'Apr',
  May = 'May',
  Jun = 'Jun',
  Jul = 'Jul',
  Aug = 'Aug',
  Sep = 'Sep',
  Oct = 'Oct',
  Nov = 'Nov',
  Dec = 'Dec',
}
daml.STATIC_IMPLEMENTS_SERIALIZABLE_CHECK<Month>(Month)
// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace Month {
  export const decoder = () => jtv.oneOf<Month>(
    jtv.constant(Month.Jan),
    jtv.constant(Month.Feb),
    jtv.constant(Month.Mar),
    jtv.constant(Month.Apr),
    jtv.constant(Month.May),
    jtv.constant(Month.Jun),
    jtv.constant(Month.Jul),
    jtv.constant(Month.Aug),
    jtv.constant(Month.Sep),
    jtv.constant(Month.Oct),
    jtv.constant(Month.Nov),
    jtv.constant(Month.Dec),
  );
}
