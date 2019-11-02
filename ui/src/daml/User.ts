import {JsonObject, JsonProperty, JsonConvert, ValueCheckingMode, OperationMode} from "json2typescript";
import { Party, TemplateId } from "../ledger/Types";

@JsonObject("User.AddFriend")
class AddFriend {
  @JsonProperty("friend", Party)
  friend: Party = '';

  static template = undefined as unknown as typeof User;

  static choiceName = 'AddFriend';

  static toJSON(addFriend: AddFriend): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<AddFriend>(addFriend);
  }
}

@JsonObject("User.RemoveFriend")
class RemoveFriend {
  @JsonProperty("friend", Party)
  friend: Party = '';

  static template = undefined as unknown as typeof User;

  static choiceName = 'RemoveFriend';

  static toJSON(removeFriend: RemoveFriend): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<RemoveFriend>(removeFriend);
  }
}

@JsonObject("User.Delete")
class Delete {
  static template = undefined as unknown as typeof User;

  static choiceName = 'Delete';

  static toJSON(delete_: Delete): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<Delete>(delete_);
  }
}

@JsonObject("User")
export class User {
  @JsonProperty("party", Party)
  party: Party = '';
  @JsonProperty("friends", [Party])
  friends: Party[] = [];

  static templateId: TemplateId = {moduleName: "User", entityName: "User"};

  static fromJSON(json: unknown): User {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    return jsonConvert.deserializeObject(json, User);
  }

  static toJSON(user: User): unknown {
    const jsonConvert = new JsonConvert();
    jsonConvert.valueCheckingMode = ValueCheckingMode.DISALLOW_NULL;
    // TODO(MH): For some reason the conversion to JSON does not work right now.
    jsonConvert.operationMode = OperationMode.DISABLE;
    return jsonConvert.serializeObject<User>(user);
  }

  static AddFriend = AddFriend;

  static RemoveFriend = RemoveFriend;

  static Delete = Delete;
}

AddFriend.template = User;

RemoveFriend.template = User;

Delete.template = User;
