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
import io.github.haibiiin.json.repair.ExtractStrategy;
import io.github.haibiiin.json.repair.antlr.KeySymbol;
import io.github.haibiiin.json.repair.antlr.autogen.JSONParser;
import java.util.List;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

public class SimpleExtractStrategy implements ExtractStrategy {
    
    @Override
    public String extract(String content, List<ParseTree> beRepairParseList, Expecting expecting) {
        if (validParseTreeList(beRepairParseList)) {
            ParseTree parseTree = beRepairParseList.get(1);
            if (parseTree instanceof JSONParser.ObjContext) {
                int start = ((JSONParser.ObjContext) parseTree).getStart().getStartIndex();
                int end = ((JSONParser.ObjContext) parseTree).getStop().getStopIndex();
                return content.substring(start, end + 1);
            }
            if (parseTree instanceof JSONParser.ArrContext) {
                int start = ((JSONParser.ArrContext) parseTree).getStart().getStartIndex();
                int end = ((JSONParser.ArrContext) parseTree).getStop().getStopIndex();
                return content.substring(start, end + 1);
            }
            if (parseTree instanceof JSONParser.ValueContext) {
                int start = ((JSONParser.ValueContext) parseTree).getStart().getStartIndex();
                int end = ((JSONParser.ValueContext) parseTree).getStop().getStopIndex();
                return content.substring(start, end + 1);
            }
            if (parseTree instanceof TerminalNode) {
                String symbol = ((TerminalNode) parseTree).getSymbol().getText();
                
                if (KeySymbol.TRUE.val().equalsIgnoreCase(symbol.toLowerCase())
                        || KeySymbol.FALSE.val().equalsIgnoreCase(symbol.toLowerCase())
                        || KeySymbol.NULL.val().equalsIgnoreCase(symbol.toLowerCase())) {
                    return symbol;
                }
                if (beRepairParseList.size() == 3
                        && beRepairParseList.get(2) instanceof TerminalNode
                        && KeySymbol.EOF.val().equalsIgnoreCase(((TerminalNode) beRepairParseList.get(2)).getSymbol().getText())) {
                    return symbol;
                }
            }
        }
        return content;
    }
    
    private boolean validParseTreeList(List<ParseTree> parseTreeList) {
        return parseTreeList.size() >= 2 && parseTreeList.get(0) instanceof JSONParser.ValueContext;
    }
}
