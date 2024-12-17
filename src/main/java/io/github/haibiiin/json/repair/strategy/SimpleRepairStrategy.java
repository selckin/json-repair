/*
 * Copyright 2024 HAibiiin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.haibiiin.json.repair.strategy;

import io.github.haibiiin.json.repair.Expecting;
import io.github.haibiiin.json.repair.RepairStrategy;
import io.github.haibiiin.json.repair.antlr.KeySymbol;
import java.util.List;
import org.antlr.v4.runtime.tree.*;

public class SimpleRepairStrategy implements RepairStrategy {
    
    @Override
    public String repair(String json, List<ParseTree> beRepairParseList, Expecting expecting) {
        Expecting.Node node = expecting.first();
        if (node.isEOF()) {
            if (endObjOrPair(node.expectingList())) {
                return json + KeySymbol.R_BRACE.val();
            }
            if (endArrOrValue(node.expectingList())) {
                return json + KeySymbol.R_BRACKET.val();
            }
            if (startPair(node.expectingList())) {
                int index = getCharPositionInLineFromErrorNode(beRepairParseList);
                return json.substring(0, index) + KeySymbol.R_BRACE.val();
            }
            if (expectingValue(node.expectingList())) {
                int index = getCharPositionInLineFromErrorNode(beRepairParseList);
                if (expectingArrValue(json, index)) {
                    return json.substring(0, index) + KeySymbol.R_BRACKET.val();
                }
                if (expectingValue(json, index)) {
                    return json + KeySymbol.NULL.val() + KeySymbol.R_BRACE.val();
                }
            }
        } else if (KeySymbol.COLON.val().equalsIgnoreCase(node.key())) {
            if (expectingEOF(node.expectingList())) {
                return KeySymbol.L_BRACE.val() + json;
            }
        } else if (this.expectingToken(node.expectingList())) {
            if (node.key().startsWith("\"")) {
                return json.replaceFirst(node.key(), node.key() + "\"");
            }
            if (node.key().endsWith("\"")) {
                return json.replaceFirst(node.key(), "\"" + node.key());
            }
            if (node.key().endsWith(KeySymbol.COLON.val())) {
                return json.replaceFirst(node.key(), "\"" + node.key().substring(0, node.key().length() - 1) + "\":");
            }
            return json.replaceFirst(node.key(), "\"" + node.key() + "\"");
        } else {
            for (ParseTree parseNode : beRepairParseList) {
                if (parseNode instanceof ErrorNode) {
                    if (node.key().equalsIgnoreCase(parseNode.getText())) {
                        if (expectingObj(node.expectingList())) {
                            int index = ((ErrorNodeImpl) parseNode).getSymbol().getCharPositionInLine();
                            if (index == json.length() - 1) {
                                return json + KeySymbol.R_BRACE.val();
                            }
                            String prefix = json.substring(0, index + 1);
                            String suffix = json.substring(index + 1);
                            return prefix + KeySymbol.R_BRACE.val() + suffix;
                        }
                        if (expectingArr(node.expectingList())) {
                            int index = ((ErrorNodeImpl) parseNode).getSymbol().getCharPositionInLine();
                            if (index == json.length() - 1) {
                                return json + KeySymbol.R_BRACKET.val();
                            }
                            String prefix = json.substring(0, index + 1);
                            String suffix = json.substring(index + 1);
                            return prefix + KeySymbol.R_BRACKET.val() + suffix;
                        }
                    }
                }
            }
        }
        return json;
    }
    
    private boolean endObjOrPair(List<String> expectingList) {
        return expectingList.size() == 2
                && expectingList.contains(KeySymbol.COMMA.val())
                && expectingList.contains(KeySymbol.R_BRACE.val());
    }
    
    private boolean endArrOrValue(List<String> expectingList) {
        return expectingList.size() == 2
                && expectingList.contains(KeySymbol.COMMA.val())
                && expectingList.contains(KeySymbol.R_BRACKET.val());
    }
    
    private boolean startPair(List<String> expectingList) {
        return expectingList.size() == 1
                && expectingList.contains(KeySymbol.STRING.val());
    }
    
    private boolean expectingValue(List<String> expectingList) {
        return expectingList.size() == 7
                && expectingList.contains(KeySymbol.L_BRACE.val())
                && expectingList.contains(KeySymbol.L_BRACKET.val())
                && expectingList.contains(KeySymbol.TRUE.val())
                && expectingList.contains(KeySymbol.FALSE.val())
                && expectingList.contains(KeySymbol.NULL.val())
                && expectingList.contains(KeySymbol.STRING.val())
                && expectingList.contains(KeySymbol.NUMBER.val());
    }
    
    private boolean expectingArrValue(String json, int index) {
        return json.substring(index).contains(KeySymbol.COMMA.val());
    }
    
    private boolean expectingValue(String json, int index) {
        return json.substring(index).contains(KeySymbol.COLON.val());
    }
    
    private boolean expectingObj(List<String> expectingList) {
        return expectingList.size() == 11;
    }
    
    private boolean expectingArr(List<String> expectingList) {
        return expectingList.size() == 7;
    }
    
    private boolean expectingEOF(List<String> expectingList) {
        return expectingList.size() == 1 && expectingList.contains(KeySymbol.EOF.val());
    }
    
    private int getCharPositionInLineFromErrorNode(List<ParseTree> beRepairParseList) {
        for (int i = beRepairParseList.size() - 1; i > 0; i--) {
            ParseTree parseNode = beRepairParseList.get(i);
            if (parseNode instanceof ErrorNode) {
                return ((ErrorNodeImpl) (beRepairParseList.get(i))).getSymbol().getCharPositionInLine();
            }
        }
        return -1;
    }
    
    private boolean expectingToken(List<String> expectingList) {
        return expectingList.size() == 1 && expectingList.contains(KeySymbol.TOKEN.val());
    }
}
