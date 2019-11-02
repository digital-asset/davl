import {JsonObject, JsonProperty, JsonConvert, ValueCheckingMode, OperationMode} from "json2typescript";
import { AnyContractId, Party, TemplateId } from "../ledger/Types";

@JsonObject("Worker")
export class Worker {
  @JsonProperty("worker", Party)
  worker: Party = '';
  @JsonProperty("company", Party)
  company: Party = '';
  @JsonProperty("requests", [AnyContractId])
  requests: AnyContractId[] = [];
  @JsonProperty("bosses", [Party])
  bosses: Party[] = [];
  @JsonProperty("minions", [Party])
  minions: Party[] = [];
  @JsonProperty("denials", [AnyContractId])
  denials: AnyContractId[] = [];

  static templateId: TemplateId = {moduleName: "Davl", entityName: "Worker"};

  static fromJSON(json: unknown): Worker {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, Worker);
  }

  static toJSON(worker: Worker): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<Worker>(worker);
  }
}

@JsonObject("AsWorker.CountUnspentHolidays")
class AsWorker_CountUnspentHolidydays {

  static template = undefined as unknown as typeof AsWorker;

  static choiceName = 'AsWorker_CountUnspentHolidays';

  static toJSON(countUnspentHolidays: AsWorker_CountUnspentHolidydays): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<AsWorker_CountUnspentHolidydays>(countUnspentHolidays);
  }
}

@JsonObject("AsWorker")
export class AsWorker {
  @JsonProperty("worker", Party)
  worker: Party = '';

  static templateId: TemplateId = {moduleName: "Davl", entityName: "AsWorker"};

  static fromJSON(json: unknown): AsWorker {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, AsWorker);
  }

  static toJSON(asWorker: AsWorker): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<AsWorker>(asWorker);
  }

  static CountUnspentHolidays = AsWorker_CountUnspentHolidydays;
}

AsWorker_CountUnspentHolidydays.template = AsWorker;
