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
                if (expectingArrValue(json)) {
                    int index = getCharPositionInLineFromErrorNode(beRepairParseList);
                    return json.substring(0, index) + KeySymbol.R_BRACKET.val();
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
    
    private boolean expectingArrValue(String json) {
        return KeySymbol.COMMA.val().equalsIgnoreCase(json.substring(json.length() - 1));
    }
    
    private int getCharPositionInLineFromErrorNode(List<ParseTree> beRepairParseList) {
        for (int i = beRepairParseList.size() - 1; i > 0; i--) {
            ParseTree parseNode = beRepairParseList.get(i);
            if (parseNode instanceof ErrorNode) {
                int charPositionInLine = ((ErrorNodeImpl) (beRepairParseList.get(i))).getSymbol().getCharPositionInLine();
                return charPositionInLine;
            }
        }
        return -1;
    }
}
