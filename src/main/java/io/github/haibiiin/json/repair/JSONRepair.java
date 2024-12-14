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
package io.github.haibiiin.json.repair;

import io.github.haibiiin.json.repair.antlr.DefaultErrorStrategyWrapper;
import io.github.haibiiin.json.repair.antlr.SyntaxErrorListener;
import io.github.haibiiin.json.repair.antlr.autogen.JSONLexer;
import io.github.haibiiin.json.repair.antlr.autogen.JSONParser;
import io.github.haibiiin.json.repair.strategy.SimpleRepairStrategy;
import java.util.List;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

public class JSONRepair {
    
    private final RepairStrategy repairStrategy;
    
    public JSONRepair() {
        this.repairStrategy = new SimpleRepairStrategy();
    }
    
    public String handle(String beRepairJSON) {
        CharStream charStream = CharStreams.fromString(beRepairJSON);
        JSONLexer lexer = new JSONLexer(charStream);
        JSONParser parser = new JSONParser(new CommonTokenStream(lexer));
        Expecting expecting = new Expecting();
        SyntaxErrorListener syntaxErrorListener = new SyntaxErrorListener(new DefaultErrorStrategyWrapper(), expecting);
        parser.addErrorListener(syntaxErrorListener);
        JSONParser.JsonContext ctx = parser.json();
        
        if (correct(expecting)) {
            return beRepairJSON;
        }
        
        List<ParseTree> beRepairParseList = ParserListBuilder.build(ctx);
        String repairJSON = repairStrategy.repair(beRepairJSON, beRepairParseList, expecting);
        
        return handle(repairJSON);
    }
    
    private boolean correct(Expecting expecting) {
        return expecting.none();
    }
    
}
