import {JsonObject, JsonProperty, JsonConvert, ValueCheckingMode, OperationMode} from "json2typescript";
import { Party, Template, TemplateId, Choice } from "../ledger/Types";

@JsonObject("EmployeeRole.RequestVacation")
export class EmployeeRole_RequestVacation {
  @JsonProperty("fromDate", String)
  fromDate: string = '';
  @JsonProperty("toDate", String)
  toDate: string = '';

  static template = undefined as unknown as typeof EmployeeRole;

  static choiceName = "EmployeeRole_RequestVacation";

  static toJSON(requestVacation: EmployeeRole_RequestVacation): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<EmployeeRole_RequestVacation>(requestVacation);
  }
}

@JsonObject("EmployeeRole")
export class EmployeeRole {
  @JsonProperty("employee", Party)
  employee: Party = '';
  @JsonProperty("company", Party)
  company: Party = '';
  @JsonProperty("boss", Party)
  boss: Party = '';

  static templateId: TemplateId = {moduleName: "DAVL", entityName: "EmployeeRole"};

  static fromJSON(json: unknown): EmployeeRole {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, EmployeeRole);
  }

  static toJSON(employeeRole: EmployeeRole): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<EmployeeRole>(employeeRole);
  }

  static RequestVacation = EmployeeRole_RequestVacation;
}

EmployeeRole_RequestVacation.template = EmployeeRole;

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _EmployeeRole: Template<EmployeeRole> = EmployeeRole;
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _EmployeeRole_RequestVacation: Choice<EmployeeRole, EmployeeRole_RequestVacation> = EmployeeRole.RequestVacation;


@JsonObject("EmployeeProposal.Accept")
export class EmployeeProposal_Accept {
  static template = undefined as unknown as typeof EmployeeProposal;

  static choiceName = "EmployeeProposal_Accept";

  static toJSON(accept: EmployeeProposal_Accept): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<EmployeeProposal_Accept>(accept);
  }
}

@JsonObject("EmployeeProposal")
export class EmployeeProposal {
  @JsonProperty("employeeRole", EmployeeRole)
  employeeRole: EmployeeRole = new EmployeeRole();
  @JsonProperty("vacationDays", String)
  vacationDays: string = '';

  static templateId: TemplateId = {moduleName: "DAVL", entityName: "EmployeeProposal"};

  static fromJSON(json: unknown): EmployeeProposal {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, EmployeeProposal);
  }

  static toJSON(employeeProposal: EmployeeProposal): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<EmployeeProposal>(employeeProposal);
  }

  static Accept = EmployeeProposal_Accept;
}

EmployeeProposal_Accept.template = EmployeeProposal;

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _EmployeeProposal: Template<EmployeeProposal> = EmployeeProposal;
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _EmployeeProposal_Accept: Choice<EmployeeProposal, EmployeeProposal_Accept> = EmployeeProposal.Accept;


@JsonObject("EmployeeVacationAllocation")
export class EmployeeVacationAllocation {
  @JsonProperty("employeeRole", EmployeeRole)
  employeeRole: EmployeeRole = new EmployeeRole();
  @JsonProperty("remainingDays", String)
  remainingDays: string = '';

  static templateId: TemplateId = {moduleName: "DAVL", entityName: "EmployeeVacationAllocation"};

  static fromJSON(json: unknown): EmployeeVacationAllocation {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, EmployeeVacationAllocation);
  }

  static toJSON(employeeVacationAllocation: EmployeeVacationAllocation): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<EmployeeVacationAllocation>(employeeVacationAllocation);
  }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _EmployeeVacationAllocation: Template<EmployeeVacationAllocation> = EmployeeVacationAllocation;


@JsonObject("Vacation")
export class Vacation {
  @JsonProperty("employeeRole", EmployeeRole)
  employeeRole: EmployeeRole = new EmployeeRole();
  @JsonProperty("fromDate", String)
  fromDate: string = '';
  @JsonProperty("toDate", String)
  toDate: string = '';

  static templateId: TemplateId = {moduleName: "DAVL", entityName: "Vacation"};

  static fromJSON(json: unknown): Vacation {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, Vacation);
  }

  static toJSON(vacation: Vacation): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<Vacation>(vacation);
  }
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _Vacation: Template<Vacation> = Vacation;


@JsonObject("VacationRequest.Accept")
export class VacationRequest_Accept {
  static template = undefined as unknown as typeof VacationRequest;

  static choiceName = "VacationRequest_Accept";

  static toJSON(accept: VacationRequest_Accept): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<VacationRequest_Accept>(accept);
  }
}

@JsonObject("VacationRequest")
export class VacationRequest {
  @JsonProperty("vacation", Vacation)
  vacation: Vacation = new Vacation();

  static templateId: TemplateId = {moduleName: "DAVL", entityName: "VacationRequest"};

  static fromJSON(json: unknown): VacationRequest {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, VacationRequest);
  }

  static toJSON(vacationRequest: VacationRequest): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<VacationRequest>(vacationRequest);
  }

  static Accept = VacationRequest_Accept;
}

VacationRequest_Accept.template = VacationRequest;

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _VacationRequest: Template<VacationRequest> = VacationRequest;
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const _VacationRequest_Accept: Choice<VacationRequest, VacationRequest_Accept> = VacationRequest.Accept;
