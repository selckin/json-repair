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
import io.github.haibiiin.json.repair.ParserListBuilder;
import io.github.haibiiin.json.repair.RepairStrategy;
import io.github.haibiiin.json.repair.antlr.autogen.JSONLexer;
import io.github.haibiiin.json.repair.antlr.autogen.JSONParser;
import java.util.List;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

public class CorrectRepairStrategy implements RepairStrategy {
    
    private List<ParseTree> correctJSONParseList;
    
    public CorrectRepairStrategy(String correctJSONRef) {
        CharStream charStream = CharStreams.fromString(correctJSONRef);
        JSONLexer lexer = new JSONLexer(charStream);
        JSONParser parser = new JSONParser(new CommonTokenStream(lexer));
        
        JSONParser.JsonContext ctx = parser.json();
        correctJSONParseList = ParserListBuilder.build(ctx);
    }
    
    @Override
    public String repair(String json, List<ParseTree> beRepairParseList, Expecting expecting) {
        return "";
    }
}
