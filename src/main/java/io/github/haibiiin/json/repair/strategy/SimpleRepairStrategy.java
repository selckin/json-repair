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
import java.util.function.BiFunction;
import java.util.function.Predicate;
import org.antlr.v4.runtime.tree.*;

public class SimpleRepairStrategy implements RepairStrategy {
    
    @Override
    public String repair(String json, List<ParseTree> beRepairParseList, Expecting expecting) {
        Expecting.Node node = expecting.first();
        SimpleNodeWrapper simpleNode = new SimpleNodeWrapper(node);
        
        FixStrategy strategy = FixStrategy.get(simpleNode);
        if (strategy != null) {
            return strategy.fixStrategy.apply(json, beRepairParseList);
        }
        
        if (simpleNode.expectingToken()) {
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
                        if (simpleNode.expectingObj()) {
                            int index = ((ErrorNodeImpl) parseNode).getSymbol().getCharPositionInLine();
                            if (index == json.length() - 1) {
                                return json + KeySymbol.R_BRACE.val();
                            }
                            String prefix = json.substring(0, index + 1);
                            String suffix = json.substring(index + 1);
                            return prefix + KeySymbol.R_BRACE.val() + suffix;
                        }
                        if (simpleNode.expectingArr()) {
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
    
    class SimpleNodeWrapper {
        
        private final Expecting.Node node;
        
        private SimpleNodeWrapper(Expecting.Node node) {
            this.node = node;
        }
        
        public boolean isEOF() {
            return KeySymbol.EOF.val().equalsIgnoreCase(node.key());
        }
        
        public boolean endObjOrPair() {
            return node.expectingList().size() == 2
                    && node.expectingList().contains(KeySymbol.COMMA.val())
                    && node.expectingList().contains(KeySymbol.R_BRACE.val());
        }
        
        public boolean endArrOrValue() {
            return node.expectingList().size() == 2
                    && node.expectingList().contains(KeySymbol.COMMA.val())
                    && node.expectingList().contains(KeySymbol.R_BRACKET.val());
        }
        
        public boolean startPair() {
            return node.expectingList().size() == 1
                    && node.expectingList().contains(KeySymbol.STRING.val());
        }
        
        public boolean expectingValue() {
            return node.expectingList().size() == 7
                    && node.expectingList().contains(KeySymbol.L_BRACE.val())
                    && node.expectingList().contains(KeySymbol.L_BRACKET.val())
                    && node.expectingList().contains(KeySymbol.TRUE.val())
                    && node.expectingList().contains(KeySymbol.FALSE.val())
                    && node.expectingList().contains(KeySymbol.NULL.val())
                    && node.expectingList().contains(KeySymbol.STRING.val())
                    && node.expectingList().contains(KeySymbol.NUMBER.val());
        }
        
        public boolean expectingObj() {
            return node.expectingList().size() == 11;
        }
        
        public boolean expectingArr() {
            return node.expectingList().size() == 7;
        }
        
        public boolean expectingEOF() {
            return KeySymbol.COLON.val().equalsIgnoreCase(node.key())
                    && node.expectingList().size() == 1
                    && node.expectingList().contains(KeySymbol.EOF.val());
        }
        
        public boolean expectingToken() {
            return node.expectingList().size() == 1 && node.expectingList().contains(KeySymbol.TOKEN.val());
        }
    }
    
    enum FixStrategy {
        
        END_OBJ_OR_PAIR(
                (SimpleNodeWrapper node) -> node.isEOF() && node.endObjOrPair(),
                (String json, List<ParseTree> beRepairParseList) -> json + KeySymbol.R_BRACE.val()),
        END_ARR_OR_VALUE(
                (SimpleNodeWrapper node) -> node.isEOF() && node.endArrOrValue(),
                (String json, List<ParseTree> beRepairParseList) -> json + KeySymbol.R_BRACKET.val()),
        START_PAIR(
                (SimpleNodeWrapper node) -> node.isEOF() && node.startPair(),
                (String json, List<ParseTree> beRepairParseList) -> {
                    int index = -1;
                    for (int i = beRepairParseList.size() - 1; i > 0; i--) {
                        ParseTree parseNode = beRepairParseList.get(i);
                        if (parseNode instanceof ErrorNode) {
                            index = ((ErrorNodeImpl) (beRepairParseList.get(i))).getSymbol().getCharPositionInLine();
                        }
                    }
                    return json.substring(0, index) + KeySymbol.R_BRACE.val();
                }),
        VALUE(
                (SimpleNodeWrapper node) -> node.isEOF() && node.expectingValue(),
                (String json, List<ParseTree> beRepairParseList) -> {
                    int index = -1;
                    for (int i = beRepairParseList.size() - 1; i > 0; i--) {
                        ParseTree parseNode = beRepairParseList.get(i);
                        if (parseNode instanceof ErrorNode) {
                            index = ((ErrorNodeImpl) (beRepairParseList.get(i))).getSymbol().getCharPositionInLine();
                        }
                    }
                    if (json.substring(index).contains(KeySymbol.COMMA.val())) {
                        return json.substring(0, index) + KeySymbol.R_BRACKET.val();
                    }
                    if (json.substring(index).contains(KeySymbol.COLON.val())) {
                        return json + KeySymbol.NULL.val() + KeySymbol.R_BRACE.val();
                    }
                    return json;
                }),
        EOF(
                SimpleNodeWrapper::expectingEOF,
                (String json, List<ParseTree> beRepairParseList) -> KeySymbol.L_BRACE.val() + json),
                ;
        
        final Predicate<SimpleNodeWrapper> expectingFixStrategy;
        final BiFunction<String, List<ParseTree>, String> fixStrategy;
        
        FixStrategy(Predicate<SimpleNodeWrapper> expectingFixStrategy, BiFunction<String, List<ParseTree>, String> fixStrategy) {
            this.expectingFixStrategy = expectingFixStrategy;
            this.fixStrategy = fixStrategy;
        }
        
        public static FixStrategy get(SimpleNodeWrapper node) {
            for (FixStrategy strategy : values()) {
                if (strategy.expectingFixStrategy.test(node)) {
                    return strategy;
                }
            }
            return null;
        }
        
    }
}
