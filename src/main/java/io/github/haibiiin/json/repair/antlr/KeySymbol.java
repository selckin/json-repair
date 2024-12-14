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
package io.github.haibiiin.json.repair.antlr;

import java.util.Arrays;
import java.util.List;

public enum KeySymbol {
    
    L_BRACE("{"),
    R_BRACE("}"),
    L_BRACKET("["),
    R_BRACKET("]"),
    COMMA(","),
    COLON(":"),
    STRING("STRING"),
    NUMBER("NUMBER"),
    TRUE("true"),
    FALSE("false"),
    NULL("null"),
    EOF("<EOF>");
    
    private String val;
    
    KeySymbol(String val) {
        this.val = val;
    }
    
    public String val() {
        return val;
    }
    
    public static List<String> value() {
        return Arrays.asList(STRING.val, NUMBER.val, TRUE.val, FALSE.val, NULL.val, L_BRACE.val, L_BRACKET.val);
    }
    
    public static List<String> obj() {
        return Arrays.asList(
                STRING.val, NUMBER.val, TRUE.val, FALSE.val, NULL.val,
                L_BRACE.val, R_BRACE.val, L_BRACKET.val, R_BRACKET.val,
                COLON.val, COMMA.val);
    }
}
