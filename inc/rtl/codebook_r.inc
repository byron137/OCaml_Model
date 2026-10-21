always @(*) begin
    case (i_q_order)
        3'd0, 3'd1: begin
            case (i_codebook_idx)
                2'd0: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd128) : begin w_symbol = 5'd 0; w_ks = 3'd7; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd128 && w_r < 8'd160) : begin w_symbol = 5'd 1; w_ks = 3'd5; w_rem = w_r - 8'd128; w_escape = 1'd0; end
                        (w_r >= 8'd160 && w_r < 8'd192) : begin w_symbol = 5'd 2; w_ks = 3'd5; w_rem = w_r - 8'd160; w_escape = 1'd0; end
                        (w_r >= 8'd192 && w_r < 8'd200) : begin w_symbol = 5'd 3; w_ks = 3'd3; w_rem = w_r - 8'd192; w_escape = 1'd0; end
                        (w_r >= 8'd200 && w_r < 8'd208) : begin w_symbol = 5'd 4; w_ks = 3'd3; w_rem = w_r - 8'd200; w_escape = 1'd0; end
                        (w_r >= 8'd208 && w_r < 8'd216) : begin w_symbol = 5'd 5; w_ks = 3'd3; w_rem = w_r - 8'd208; w_escape = 1'd0; end
                        (w_r >= 8'd216 && w_r < 8'd224) : begin w_symbol = 5'd 6; w_ks = 3'd3; w_rem = w_r - 8'd216; w_escape = 1'd0; end
                        (w_r >= 8'd224 && w_r < 8'd226) : begin w_symbol = 5'd 7; w_ks = 3'd1; w_rem = w_r - 8'd224; w_escape = 1'd0; end
                        (w_r >= 8'd226 && w_r < 8'd228) : begin w_symbol = 5'd 8; w_ks = 3'd1; w_rem = w_r - 8'd226; w_escape = 1'd0; end
                        (w_r >= 8'd228 && w_r < 8'd230) : begin w_symbol = 5'd 9; w_ks = 3'd1; w_rem = w_r - 8'd228; w_escape = 1'd0; end
                        (w_r >= 8'd230 && w_r < 8'd232) : begin w_symbol = 5'd10; w_ks = 3'd1; w_rem = w_r - 8'd230; w_escape = 1'd0; end
                        (w_r >= 8'd232 && w_r < 8'd234) : begin w_symbol = 5'd11; w_ks = 3'd1; w_rem = w_r - 8'd232; w_escape = 1'd0; end
                        (w_r >= 8'd234 && w_r < 8'd236) : begin w_symbol = 5'd12; w_ks = 3'd1; w_rem = w_r - 8'd234; w_escape = 1'd0; end
                        (w_r == 8'd236)                 : begin w_symbol = 5'd13; w_ks = 3'd0; w_rem = w_r - 8'd236; w_escape = 1'd0; end
                        (w_r == 8'd237)                 : begin w_symbol = 5'd14; w_ks = 3'd0; w_rem = w_r - 8'd237; w_escape = 1'd0; end
                        (w_r == 8'd238)                 : begin w_symbol = 5'd15; w_ks = 3'd0; w_rem = w_r - 8'd238; w_escape = 1'd0; end
                        (w_r == 8'd239)                 : begin w_symbol = 5'd16; w_ks = 3'd0; w_rem = w_r - 8'd239; w_escape = 1'd0; end
                        (w_r == 8'd240)                 : begin w_symbol = 5'd17; w_ks = 3'd0; w_rem = w_r - 8'd240; w_escape = 1'd0; end
                        (w_r == 8'd241)                 : begin w_symbol = 5'd18; w_ks = 3'd0; w_rem = w_r - 8'd241; w_escape = 1'd0; end
                        (w_r == 8'd242)                 : begin w_symbol = 5'd19; w_ks = 3'd0; w_rem = w_r - 8'd242; w_escape = 1'd0; end
                        (w_r == 8'd243)                 : begin w_symbol = 5'd20; w_ks = 3'd0; w_rem = w_r - 8'd243; w_escape = 1'd0; end
                        (w_r == 8'd244)                 : begin w_symbol = 5'd21; w_ks = 3'd0; w_rem = w_r - 8'd244; w_escape = 1'd0; end
                        (w_r == 8'd245)                 : begin w_symbol = 5'd22; w_ks = 3'd0; w_rem = w_r - 8'd245; w_escape = 1'd0; end
                        (w_r == 8'd246)                 : begin w_symbol = 5'd23; w_ks = 3'd0; w_rem = w_r - 8'd246; w_escape = 1'd0; end
                        (w_r == 8'd247)                 : begin w_symbol = 5'd24; w_ks = 3'd0; w_rem = w_r - 8'd247; w_escape = 1'd0; end
                        (w_r == 8'd248)                 : begin w_symbol = 5'd25; w_ks = 3'd0; w_rem = w_r - 8'd248; w_escape = 1'd0; end
                        (w_r == 8'd249)                 : begin w_symbol = 5'd26; w_ks = 3'd0; w_rem = w_r - 8'd249; w_escape = 1'd0; end
                        (w_r == 8'd250)                 : begin w_symbol = 5'd27; w_ks = 3'd0; w_rem = w_r - 8'd250; w_escape = 1'd0; end
                        (w_r == 8'd251)                 : begin w_symbol = 5'd28; w_ks = 3'd0; w_rem = w_r - 8'd251; w_escape = 1'd0; end
                        (w_r == 8'd252)                 : begin w_symbol = 5'd29; w_ks = 3'd0; w_rem = w_r - 8'd252; w_escape = 1'd0; end
                        (w_r == 8'd253)                 : begin w_symbol = 5'd30; w_ks = 3'd0; w_rem = w_r - 8'd253; w_escape = 1'd0; end
                        (w_r == 8'd254)                 : begin w_symbol = 5'd31; w_ks = 3'd0; w_rem = w_r - 8'd254; w_escape = 1'd0; end
                        (w_r == 8'd255)                 : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd255; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd1: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd32)  : begin w_symbol = 5'd 0; w_ks = 3'd5; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd64)  : begin w_symbol = 5'd 1; w_ks = 3'd5; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd96)  : begin w_symbol = 5'd 2; w_ks = 3'd5; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd128) : begin w_symbol = 5'd 3; w_ks = 3'd5; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd128 && w_r < 8'd160) : begin w_symbol = 5'd 4; w_ks = 3'd5; w_rem = w_r - 8'd128; w_escape = 1'd0; end
                        (w_r >= 8'd160 && w_r < 8'd192) : begin w_symbol = 5'd 5; w_ks = 3'd5; w_rem = w_r - 8'd160; w_escape = 1'd0; end
                        (w_r >= 8'd192 && w_r < 8'd224) : begin w_symbol = 5'd 6; w_ks = 3'd5; w_rem = w_r - 8'd192; w_escape = 1'd0; end
                        (w_r >= 8'd224 && w_r < 8'd226) : begin w_symbol = 5'd 7; w_ks = 3'd1; w_rem = w_r - 8'd224; w_escape = 1'd0; end
                        (w_r >= 8'd226 && w_r < 8'd228) : begin w_symbol = 5'd 8; w_ks = 3'd1; w_rem = w_r - 8'd226; w_escape = 1'd0; end
                        (w_r >= 8'd228 && w_r < 8'd230) : begin w_symbol = 5'd 9; w_ks = 3'd1; w_rem = w_r - 8'd228; w_escape = 1'd0; end
                        (w_r >= 8'd230 && w_r < 8'd232) : begin w_symbol = 5'd10; w_ks = 3'd1; w_rem = w_r - 8'd230; w_escape = 1'd0; end
                        (w_r >= 8'd232 && w_r < 8'd234) : begin w_symbol = 5'd11; w_ks = 3'd1; w_rem = w_r - 8'd232; w_escape = 1'd0; end
                        (w_r >= 8'd234 && w_r < 8'd236) : begin w_symbol = 5'd12; w_ks = 3'd1; w_rem = w_r - 8'd234; w_escape = 1'd0; end
                        (w_r == 8'd236)                 : begin w_symbol = 5'd13; w_ks = 3'd0; w_rem = w_r - 8'd236; w_escape = 1'd0; end
                        (w_r == 8'd237)                 : begin w_symbol = 5'd14; w_ks = 3'd0; w_rem = w_r - 8'd237; w_escape = 1'd0; end
                        (w_r == 8'd238)                 : begin w_symbol = 5'd15; w_ks = 3'd0; w_rem = w_r - 8'd238; w_escape = 1'd0; end
                        (w_r == 8'd239)                 : begin w_symbol = 5'd16; w_ks = 3'd0; w_rem = w_r - 8'd239; w_escape = 1'd0; end
                        (w_r == 8'd240)                 : begin w_symbol = 5'd17; w_ks = 3'd0; w_rem = w_r - 8'd240; w_escape = 1'd0; end
                        (w_r == 8'd241)                 : begin w_symbol = 5'd18; w_ks = 3'd0; w_rem = w_r - 8'd241; w_escape = 1'd0; end
                        (w_r == 8'd242)                 : begin w_symbol = 5'd19; w_ks = 3'd0; w_rem = w_r - 8'd242; w_escape = 1'd0; end
                        (w_r == 8'd243)                 : begin w_symbol = 5'd20; w_ks = 3'd0; w_rem = w_r - 8'd243; w_escape = 1'd0; end
                        (w_r == 8'd244)                 : begin w_symbol = 5'd21; w_ks = 3'd0; w_rem = w_r - 8'd244; w_escape = 1'd0; end
                        (w_r == 8'd245)                 : begin w_symbol = 5'd22; w_ks = 3'd0; w_rem = w_r - 8'd245; w_escape = 1'd0; end
                        (w_r == 8'd246)                 : begin w_symbol = 5'd23; w_ks = 3'd0; w_rem = w_r - 8'd246; w_escape = 1'd0; end
                        (w_r == 8'd247)                 : begin w_symbol = 5'd24; w_ks = 3'd0; w_rem = w_r - 8'd247; w_escape = 1'd0; end
                        (w_r == 8'd248)                 : begin w_symbol = 5'd25; w_ks = 3'd0; w_rem = w_r - 8'd248; w_escape = 1'd0; end
                        (w_r == 8'd249)                 : begin w_symbol = 5'd26; w_ks = 3'd0; w_rem = w_r - 8'd249; w_escape = 1'd0; end
                        (w_r == 8'd250)                 : begin w_symbol = 5'd27; w_ks = 3'd0; w_rem = w_r - 8'd250; w_escape = 1'd0; end
                        (w_r == 8'd251)                 : begin w_symbol = 5'd28; w_ks = 3'd0; w_rem = w_r - 8'd251; w_escape = 1'd0; end
                        (w_r == 8'd252)                 : begin w_symbol = 5'd29; w_ks = 3'd0; w_rem = w_r - 8'd252; w_escape = 1'd0; end
                        (w_r == 8'd253)                 : begin w_symbol = 5'd30; w_ks = 3'd0; w_rem = w_r - 8'd253; w_escape = 1'd0; end
                        (w_r == 8'd254)                 : begin w_symbol = 5'd31; w_ks = 3'd0; w_rem = w_r - 8'd254; w_escape = 1'd0; end
                        (w_r == 8'd255)                 : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd255; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd2: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd32)  : begin w_symbol = 5'd 0; w_ks = 3'd5; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd34)  : begin w_symbol = 5'd 1; w_ks = 3'd1; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd34  && w_r < 8'd36)  : begin w_symbol = 5'd 2; w_ks = 3'd1; w_rem = w_r - 8'd34 ; w_escape = 1'd0; end
                        (w_r >= 8'd36  && w_r < 8'd38)  : begin w_symbol = 5'd 3; w_ks = 3'd1; w_rem = w_r - 8'd36 ; w_escape = 1'd0; end
                        (w_r >= 8'd38  && w_r < 8'd40)  : begin w_symbol = 5'd 4; w_ks = 3'd1; w_rem = w_r - 8'd38 ; w_escape = 1'd0; end
                        (w_r >= 8'd40  && w_r < 8'd42)  : begin w_symbol = 5'd 5; w_ks = 3'd1; w_rem = w_r - 8'd40 ; w_escape = 1'd0; end
                        (w_r >= 8'd42  && w_r < 8'd44)  : begin w_symbol = 5'd 6; w_ks = 3'd1; w_rem = w_r - 8'd42 ; w_escape = 1'd0; end
                        (w_r >= 8'd44  && w_r < 8'd76)  : begin w_symbol = 5'd 7; w_ks = 3'd5; w_rem = w_r - 8'd44 ; w_escape = 1'd0; end
                        (w_r >= 8'd76  && w_r < 8'd108) : begin w_symbol = 5'd 8; w_ks = 3'd5; w_rem = w_r - 8'd76 ; w_escape = 1'd0; end
                        (w_r >= 8'd108 && w_r < 8'd140) : begin w_symbol = 5'd 9; w_ks = 3'd5; w_rem = w_r - 8'd108; w_escape = 1'd0; end
                        (w_r >= 8'd140 && w_r < 8'd172) : begin w_symbol = 5'd10; w_ks = 3'd5; w_rem = w_r - 8'd140; w_escape = 1'd0; end
                        (w_r >= 8'd172 && w_r < 8'd204) : begin w_symbol = 5'd11; w_ks = 3'd5; w_rem = w_r - 8'd172; w_escape = 1'd0; end
                        (w_r >= 8'd204 && w_r < 8'd236) : begin w_symbol = 5'd12; w_ks = 3'd5; w_rem = w_r - 8'd204; w_escape = 1'd0; end
                        (w_r == 8'd236)                 : begin w_symbol = 5'd13; w_ks = 3'd0; w_rem = w_r - 8'd236; w_escape = 1'd0; end
                        (w_r == 8'd237)                 : begin w_symbol = 5'd14; w_ks = 3'd0; w_rem = w_r - 8'd237; w_escape = 1'd0; end
                        (w_r == 8'd238)                 : begin w_symbol = 5'd15; w_ks = 3'd0; w_rem = w_r - 8'd238; w_escape = 1'd0; end
                        (w_r == 8'd239)                 : begin w_symbol = 5'd16; w_ks = 3'd0; w_rem = w_r - 8'd239; w_escape = 1'd0; end
                        (w_r == 8'd240)                 : begin w_symbol = 5'd17; w_ks = 3'd0; w_rem = w_r - 8'd240; w_escape = 1'd0; end
                        (w_r == 8'd241)                 : begin w_symbol = 5'd18; w_ks = 3'd0; w_rem = w_r - 8'd241; w_escape = 1'd0; end
                        (w_r == 8'd242)                 : begin w_symbol = 5'd19; w_ks = 3'd0; w_rem = w_r - 8'd242; w_escape = 1'd0; end
                        (w_r == 8'd243)                 : begin w_symbol = 5'd20; w_ks = 3'd0; w_rem = w_r - 8'd243; w_escape = 1'd0; end
                        (w_r == 8'd244)                 : begin w_symbol = 5'd21; w_ks = 3'd0; w_rem = w_r - 8'd244; w_escape = 1'd0; end
                        (w_r == 8'd245)                 : begin w_symbol = 5'd22; w_ks = 3'd0; w_rem = w_r - 8'd245; w_escape = 1'd0; end
                        (w_r == 8'd246)                 : begin w_symbol = 5'd23; w_ks = 3'd0; w_rem = w_r - 8'd246; w_escape = 1'd0; end
                        (w_r == 8'd247)                 : begin w_symbol = 5'd24; w_ks = 3'd0; w_rem = w_r - 8'd247; w_escape = 1'd0; end
                        (w_r == 8'd248)                 : begin w_symbol = 5'd25; w_ks = 3'd0; w_rem = w_r - 8'd248; w_escape = 1'd0; end
                        (w_r == 8'd249)                 : begin w_symbol = 5'd26; w_ks = 3'd0; w_rem = w_r - 8'd249; w_escape = 1'd0; end
                        (w_r == 8'd250)                 : begin w_symbol = 5'd27; w_ks = 3'd0; w_rem = w_r - 8'd250; w_escape = 1'd0; end
                        (w_r == 8'd251)                 : begin w_symbol = 5'd28; w_ks = 3'd0; w_rem = w_r - 8'd251; w_escape = 1'd0; end
                        (w_r == 8'd252)                 : begin w_symbol = 5'd29; w_ks = 3'd0; w_rem = w_r - 8'd252; w_escape = 1'd0; end
                        (w_r == 8'd253)                 : begin w_symbol = 5'd30; w_ks = 3'd0; w_rem = w_r - 8'd253; w_escape = 1'd0; end
                        (w_r == 8'd254)                 : begin w_symbol = 5'd31; w_ks = 3'd0; w_rem = w_r - 8'd254; w_escape = 1'd0; end
                        (w_r == 8'd255)                 : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd255; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd3: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd8)   : begin w_symbol = 5'd 0; w_ks = 3'd3; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd8   && w_r < 8'd16)  : begin w_symbol = 5'd 1; w_ks = 3'd3; w_rem = w_r - 8'd8  ; w_escape = 1'd0; end
                        (w_r >= 8'd16  && w_r < 8'd24)  : begin w_symbol = 5'd 2; w_ks = 3'd3; w_rem = w_r - 8'd16 ; w_escape = 1'd0; end
                        (w_r >= 8'd24  && w_r < 8'd32)  : begin w_symbol = 5'd 3; w_ks = 3'd3; w_rem = w_r - 8'd24 ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd40)  : begin w_symbol = 5'd 4; w_ks = 3'd3; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd40  && w_r < 8'd48)  : begin w_symbol = 5'd 5; w_ks = 3'd3; w_rem = w_r - 8'd40 ; w_escape = 1'd0; end
                        (w_r >= 8'd48  && w_r < 8'd56)  : begin w_symbol = 5'd 6; w_ks = 3'd3; w_rem = w_r - 8'd48 ; w_escape = 1'd0; end
                        (w_r >= 8'd56  && w_r < 8'd64)  : begin w_symbol = 5'd 7; w_ks = 3'd3; w_rem = w_r - 8'd56 ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd72)  : begin w_symbol = 5'd 8; w_ks = 3'd3; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd72  && w_r < 8'd80)  : begin w_symbol = 5'd 9; w_ks = 3'd3; w_rem = w_r - 8'd72 ; w_escape = 1'd0; end
                        (w_r >= 8'd80  && w_r < 8'd88)  : begin w_symbol = 5'd10; w_ks = 3'd3; w_rem = w_r - 8'd80 ; w_escape = 1'd0; end
                        (w_r >= 8'd88  && w_r < 8'd96)  : begin w_symbol = 5'd11; w_ks = 3'd3; w_rem = w_r - 8'd88 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd104) : begin w_symbol = 5'd12; w_ks = 3'd3; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd104 && w_r < 8'd112) : begin w_symbol = 5'd13; w_ks = 3'd3; w_rem = w_r - 8'd104; w_escape = 1'd0; end
                        (w_r >= 8'd112 && w_r < 8'd120) : begin w_symbol = 5'd14; w_ks = 3'd3; w_rem = w_r - 8'd112; w_escape = 1'd0; end
                        (w_r >= 8'd120 && w_r < 8'd128) : begin w_symbol = 5'd15; w_ks = 3'd3; w_rem = w_r - 8'd120; w_escape = 1'd0; end
                        (w_r >= 8'd128 && w_r < 8'd136) : begin w_symbol = 5'd16; w_ks = 3'd3; w_rem = w_r - 8'd128; w_escape = 1'd0; end
                        (w_r >= 8'd136 && w_r < 8'd144) : begin w_symbol = 5'd17; w_ks = 3'd3; w_rem = w_r - 8'd136; w_escape = 1'd0; end
                        (w_r >= 8'd144 && w_r < 8'd152) : begin w_symbol = 5'd18; w_ks = 3'd3; w_rem = w_r - 8'd144; w_escape = 1'd0; end
                        (w_r >= 8'd152 && w_r < 8'd160) : begin w_symbol = 5'd19; w_ks = 3'd3; w_rem = w_r - 8'd152; w_escape = 1'd0; end
                        (w_r >= 8'd160 && w_r < 8'd168) : begin w_symbol = 5'd20; w_ks = 3'd3; w_rem = w_r - 8'd160; w_escape = 1'd0; end
                        (w_r >= 8'd168 && w_r < 8'd176) : begin w_symbol = 5'd21; w_ks = 3'd3; w_rem = w_r - 8'd168; w_escape = 1'd0; end
                        (w_r >= 8'd176 && w_r < 8'd184) : begin w_symbol = 5'd22; w_ks = 3'd3; w_rem = w_r - 8'd176; w_escape = 1'd0; end
                        (w_r >= 8'd184 && w_r < 8'd192) : begin w_symbol = 5'd23; w_ks = 3'd3; w_rem = w_r - 8'd184; w_escape = 1'd0; end
                        (w_r >= 8'd192 && w_r < 8'd200) : begin w_symbol = 5'd24; w_ks = 3'd3; w_rem = w_r - 8'd192; w_escape = 1'd0; end
                        (w_r >= 8'd200 && w_r < 8'd208) : begin w_symbol = 5'd25; w_ks = 3'd3; w_rem = w_r - 8'd200; w_escape = 1'd0; end
                        (w_r >= 8'd208 && w_r < 8'd216) : begin w_symbol = 5'd26; w_ks = 3'd3; w_rem = w_r - 8'd208; w_escape = 1'd0; end
                        (w_r >= 8'd216 && w_r < 8'd224) : begin w_symbol = 5'd27; w_ks = 3'd3; w_rem = w_r - 8'd216; w_escape = 1'd0; end
                        (w_r >= 8'd224 && w_r < 8'd232) : begin w_symbol = 5'd28; w_ks = 3'd3; w_rem = w_r - 8'd224; w_escape = 1'd0; end
                        (w_r >= 8'd232 && w_r < 8'd240) : begin w_symbol = 5'd29; w_ks = 3'd3; w_rem = w_r - 8'd232; w_escape = 1'd0; end
                        (w_r >= 8'd240 && w_r < 8'd248) : begin w_symbol = 5'd30; w_ks = 3'd3; w_rem = w_r - 8'd240; w_escape = 1'd0; end
                        (w_r >= 8'd248 && w_r < 8'd252) : begin w_symbol = 5'd31; w_ks = 3'd2; w_rem = w_r - 8'd248; w_escape = 1'd0; end
                        (w_r >= 8'd252 && w_r <= 8'd255): begin w_symbol = 5'd 0; w_ks = 3'd2; w_rem = w_r - 8'd252; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end
            endcase
        end

        3'd2, 3'd3: begin
            case (i_codebook_idx)
                2'd0: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd64)  : begin w_symbol = 5'd 0; w_ks = 3'd6; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd80)  : begin w_symbol = 5'd 1; w_ks = 3'd4; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd80  && w_r < 8'd96)  : begin w_symbol = 5'd 2; w_ks = 3'd4; w_rem = w_r - 8'd80 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd100) : begin w_symbol = 5'd 3; w_ks = 3'd2; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd100 && w_r < 8'd104) : begin w_symbol = 5'd 4; w_ks = 3'd2; w_rem = w_r - 8'd100; w_escape = 1'd0; end
                        (w_r >= 8'd104 && w_r < 8'd108) : begin w_symbol = 5'd 5; w_ks = 3'd2; w_rem = w_r - 8'd104; w_escape = 1'd0; end
                        (w_r >= 8'd108 && w_r < 8'd112) : begin w_symbol = 5'd 6; w_ks = 3'd2; w_rem = w_r - 8'd108; w_escape = 1'd0; end
                        (w_r >= 8'd112 && w_r < 8'd114) : begin w_symbol = 5'd 7; w_ks = 3'd1; w_rem = w_r - 8'd112; w_escape = 1'd0; end
                        (w_r >= 8'd114 && w_r < 8'd116) : begin w_symbol = 5'd 8; w_ks = 3'd1; w_rem = w_r - 8'd114; w_escape = 1'd0; end
                        (w_r >= 8'd116 && w_r < 8'd118) : begin w_symbol = 5'd 9; w_ks = 3'd1; w_rem = w_r - 8'd116; w_escape = 1'd0; end
                        (w_r >= 8'd118 && w_r < 8'd120) : begin w_symbol = 5'd10; w_ks = 3'd1; w_rem = w_r - 8'd118; w_escape = 1'd0; end
                        (w_r >= 8'd120 && w_r < 8'd122) : begin w_symbol = 5'd11; w_ks = 3'd1; w_rem = w_r - 8'd120; w_escape = 1'd0; end
                        (w_r >= 8'd122 && w_r < 8'd124) : begin w_symbol = 5'd12; w_ks = 3'd1; w_rem = w_r - 8'd122; w_escape = 1'd0; end
                        (w_r == 8'd124)                 : begin w_symbol = 5'd13; w_ks = 3'd0; w_rem = w_r - 8'd124; w_escape = 1'd0; end
                        (w_r == 8'd125)                 : begin w_symbol = 5'd14; w_ks = 3'd0; w_rem = w_r - 8'd125; w_escape = 1'd0; end
                        (w_r == 8'd126)                 : begin w_symbol = 5'd15; w_ks = 3'd0; w_rem = w_r - 8'd126; w_escape = 1'd0; end
                        (w_r == 8'd127)                 : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd127; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd1: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd16)  : begin w_symbol = 5'd 0; w_ks = 3'd4; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd16  && w_r < 8'd32)  : begin w_symbol = 5'd 1; w_ks = 3'd4; w_rem = w_r - 8'd16 ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd48)  : begin w_symbol = 5'd 2; w_ks = 3'd4; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd48  && w_r < 8'd64)  : begin w_symbol = 5'd 3; w_ks = 3'd4; w_rem = w_r - 8'd48 ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd80)  : begin w_symbol = 5'd 4; w_ks = 3'd4; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd80  && w_r < 8'd96)  : begin w_symbol = 5'd 5; w_ks = 3'd4; w_rem = w_r - 8'd80 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd112) : begin w_symbol = 5'd 6; w_ks = 3'd4; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd112 && w_r < 8'd114) : begin w_symbol = 5'd 7; w_ks = 3'd1; w_rem = w_r - 8'd112; w_escape = 1'd0; end
                        (w_r >= 8'd114 && w_r < 8'd116) : begin w_symbol = 5'd 8; w_ks = 3'd1; w_rem = w_r - 8'd114; w_escape = 1'd0; end
                        (w_r >= 8'd116 && w_r < 8'd118) : begin w_symbol = 5'd 9; w_ks = 3'd1; w_rem = w_r - 8'd116; w_escape = 1'd0; end
                        (w_r >= 8'd118 && w_r < 8'd120) : begin w_symbol = 5'd10; w_ks = 3'd1; w_rem = w_r - 8'd118; w_escape = 1'd0; end
                        (w_r >= 8'd120 && w_r < 8'd122) : begin w_symbol = 5'd11; w_ks = 3'd1; w_rem = w_r - 8'd120; w_escape = 1'd0; end
                        (w_r >= 8'd122 && w_r < 8'd124) : begin w_symbol = 5'd12; w_ks = 3'd1; w_rem = w_r - 8'd122; w_escape = 1'd0; end
                        (w_r == 8'd124)                 : begin w_symbol = 5'd13; w_ks = 3'd0; w_rem = w_r - 8'd124; w_escape = 1'd0; end
                        (w_r == 8'd125)                 : begin w_symbol = 5'd14; w_ks = 3'd0; w_rem = w_r - 8'd125; w_escape = 1'd0; end
                        (w_r == 8'd126)                 : begin w_symbol = 5'd15; w_ks = 3'd0; w_rem = w_r - 8'd126; w_escape = 1'd0; end
                        (w_r == 8'd127)                 : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd127; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd2: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd32)  : begin w_symbol = 5'd 0; w_ks = 3'd5; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd48)  : begin w_symbol = 5'd 1; w_ks = 3'd4; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd48  && w_r < 8'd64)  : begin w_symbol = 5'd 2; w_ks = 3'd4; w_rem = w_r - 8'd48 ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd72)  : begin w_symbol = 5'd 3; w_ks = 3'd3; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd72  && w_r < 8'd80)  : begin w_symbol = 5'd 4; w_ks = 3'd3; w_rem = w_r - 8'd72 ; w_escape = 1'd0; end
                        (w_r >= 8'd80  && w_r < 8'd88)  : begin w_symbol = 5'd 5; w_ks = 3'd3; w_rem = w_r - 8'd80 ; w_escape = 1'd0; end
                        (w_r >= 8'd88  && w_r < 8'd96)  : begin w_symbol = 5'd 6; w_ks = 3'd3; w_rem = w_r - 8'd88 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd100) : begin w_symbol = 5'd 7; w_ks = 3'd2; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd100 && w_r < 8'd104) : begin w_symbol = 5'd 8; w_ks = 3'd2; w_rem = w_r - 8'd100; w_escape = 1'd0; end
                        (w_r >= 8'd104 && w_r < 8'd108) : begin w_symbol = 5'd 9; w_ks = 3'd2; w_rem = w_r - 8'd104; w_escape = 1'd0; end
                        (w_r >= 8'd108 && w_r < 8'd112) : begin w_symbol = 5'd10; w_ks = 3'd2; w_rem = w_r - 8'd108; w_escape = 1'd0; end
                        (w_r >= 8'd112 && w_r < 8'd116) : begin w_symbol = 5'd11; w_ks = 3'd2; w_rem = w_r - 8'd112; w_escape = 1'd0; end
                        (w_r >= 8'd116 && w_r < 8'd120) : begin w_symbol = 5'd12; w_ks = 3'd2; w_rem = w_r - 8'd116; w_escape = 1'd0; end
                        (w_r >= 8'd120 && w_r < 8'd122) : begin w_symbol = 5'd13; w_ks = 3'd1; w_rem = w_r - 8'd120; w_escape = 1'd0; end
                        (w_r >= 8'd122 && w_r < 8'd124) : begin w_symbol = 5'd14; w_ks = 3'd1; w_rem = w_r - 8'd122; w_escape = 1'd0; end
                        (w_r >= 8'd124 && w_r < 8'd126) : begin w_symbol = 5'd15; w_ks = 3'd1; w_rem = w_r - 8'd124; w_escape = 1'd0; end
                        (w_r >= 8'd126 && w_r < 8'd128) : begin w_symbol = 5'd 0; w_ks = 3'd1; w_rem = w_r - 8'd126; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end

                2'd3: begin
                    case (1'b1)
                        (w_r >= 8'd0   && w_r < 8'd8)   : begin w_symbol = 5'd 0; w_ks = 3'd3; w_rem = w_r - 8'd0  ; w_escape = 1'd0; end
                        (w_r >= 8'd8   && w_r < 8'd16)  : begin w_symbol = 5'd 1; w_ks = 3'd3; w_rem = w_r - 8'd8  ; w_escape = 1'd0; end
                        (w_r >= 8'd16  && w_r < 8'd24)  : begin w_symbol = 5'd 2; w_ks = 3'd3; w_rem = w_r - 8'd16 ; w_escape = 1'd0; end
                        (w_r >= 8'd24  && w_r < 8'd32)  : begin w_symbol = 5'd 3; w_ks = 3'd3; w_rem = w_r - 8'd24 ; w_escape = 1'd0; end
                        (w_r >= 8'd32  && w_r < 8'd40)  : begin w_symbol = 5'd 4; w_ks = 3'd3; w_rem = w_r - 8'd32 ; w_escape = 1'd0; end
                        (w_r >= 8'd40  && w_r < 8'd48)  : begin w_symbol = 5'd 5; w_ks = 3'd3; w_rem = w_r - 8'd40 ; w_escape = 1'd0; end
                        (w_r >= 8'd48  && w_r < 8'd56)  : begin w_symbol = 5'd 6; w_ks = 3'd3; w_rem = w_r - 8'd48 ; w_escape = 1'd0; end
                        (w_r >= 8'd56  && w_r < 8'd64)  : begin w_symbol = 5'd 7; w_ks = 3'd3; w_rem = w_r - 8'd56 ; w_escape = 1'd0; end
                        (w_r >= 8'd64  && w_r < 8'd72)  : begin w_symbol = 5'd 8; w_ks = 3'd3; w_rem = w_r - 8'd64 ; w_escape = 1'd0; end
                        (w_r >= 8'd72  && w_r < 8'd80)  : begin w_symbol = 5'd 9; w_ks = 3'd3; w_rem = w_r - 8'd72 ; w_escape = 1'd0; end
                        (w_r >= 8'd80  && w_r < 8'd88)  : begin w_symbol = 5'd10; w_ks = 3'd3; w_rem = w_r - 8'd80 ; w_escape = 1'd0; end
                        (w_r >= 8'd88  && w_r < 8'd96)  : begin w_symbol = 5'd11; w_ks = 3'd3; w_rem = w_r - 8'd88 ; w_escape = 1'd0; end
                        (w_r >= 8'd96  && w_r < 8'd104) : begin w_symbol = 5'd12; w_ks = 3'd3; w_rem = w_r - 8'd96 ; w_escape = 1'd0; end
                        (w_r >= 8'd104 && w_r < 8'd112) : begin w_symbol = 5'd13; w_ks = 3'd3; w_rem = w_r - 8'd104; w_escape = 1'd0; end
                        (w_r >= 8'd112 && w_r < 8'd120) : begin w_symbol = 5'd14; w_ks = 3'd3; w_rem = w_r - 8'd112; w_escape = 1'd0; end
                        (w_r >= 8'd120 && w_r < 8'd124) : begin w_symbol = 5'd15; w_ks = 3'd2; w_rem = w_r - 8'd120; w_escape = 1'd0; end
                        (w_r >= 8'd124 && w_r < 8'd128) : begin w_symbol = 5'd 0; w_ks = 3'd2; w_rem = w_r - 8'd124; w_escape = 1'd1; end
                        default                         : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;         w_escape = 1'd0; end
                    endcase
                end
            endcase
        end

        3'd4, 3'd5: begin
            case (i_codebook_idx)
                2'd0: begin
                    case (1'b1)
                        (w_r >= 8'd0  && w_r < 8'd16) : begin w_symbol = 5'd 0; w_ks = 3'd4; w_rem = w_r - 8'd0 ; w_escape = 1'd0; end
                        (w_r >= 8'd16 && w_r < 8'd20) : begin w_symbol = 5'd 1; w_ks = 3'd2; w_rem = w_r - 8'd16; w_escape = 1'd0; end
                        (w_r >= 8'd20 && w_r < 8'd24) : begin w_symbol = 5'd 2; w_ks = 3'd2; w_rem = w_r - 8'd20; w_escape = 1'd0; end
                        (w_r >= 8'd24 && w_r < 8'd26) : begin w_symbol = 5'd 3; w_ks = 3'd1; w_rem = w_r - 8'd24; w_escape = 1'd0; end
                        (w_r >= 8'd26 && w_r < 8'd28) : begin w_symbol = 5'd 4; w_ks = 3'd1; w_rem = w_r - 8'd26; w_escape = 1'd0; end
                        (w_r == 8'd28)                : begin w_symbol = 5'd 5; w_ks = 3'd0; w_rem = w_r - 8'd28; w_escape = 1'd0; end
                        (w_r == 8'd29)                : begin w_symbol = 5'd 6; w_ks = 3'd0; w_rem = w_r - 8'd29; w_escape = 1'd0; end
                        (w_r == 8'd30)                : begin w_symbol = 5'd 7; w_ks = 3'd0; w_rem = w_r - 8'd30; w_escape = 1'd0; end
                        (w_r == 8'd31)                : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd31; w_escape = 1'd1; end
                        default                       : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;        w_escape = 1'd0; end
                    endcase
                end

                2'd1: begin
                    case (1'b1)
                        (w_r >= 8'd0  && w_r < 8'd8)  : begin w_symbol = 5'd 0; w_ks = 3'd3; w_rem = w_r - 8'd0 ; w_escape = 1'd0; end
                        (w_r >= 8'd8  && w_r < 8'd16) : begin w_symbol = 5'd 1; w_ks = 3'd3; w_rem = w_r - 8'd8 ; w_escape = 1'd0; end
                        (w_r >= 8'd16 && w_r < 8'd24) : begin w_symbol = 5'd 2; w_ks = 3'd3; w_rem = w_r - 8'd16; w_escape = 1'd0; end
                        (w_r >= 8'd24 && w_r < 8'd26) : begin w_symbol = 5'd 3; w_ks = 3'd1; w_rem = w_r - 8'd24; w_escape = 1'd0; end
                        (w_r >= 8'd26 && w_r < 8'd28) : begin w_symbol = 5'd 4; w_ks = 3'd1; w_rem = w_r - 8'd26; w_escape = 1'd0; end
                        (w_r == 8'd28)                : begin w_symbol = 5'd 5; w_ks = 3'd0; w_rem = w_r - 8'd28; w_escape = 1'd0; end
                        (w_r == 8'd29)                : begin w_symbol = 5'd 6; w_ks = 3'd0; w_rem = w_r - 8'd29; w_escape = 1'd0; end
                        (w_r == 8'd30)                : begin w_symbol = 5'd 7; w_ks = 3'd0; w_rem = w_r - 8'd30; w_escape = 1'd0; end
                        (w_r == 8'd31)                : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd31; w_escape = 1'd1; end
                        default                       : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;        w_escape = 1'd0; end
                    endcase
                end

                2'd2: begin
                    case (1'b1)
                        (w_r >= 8'd0  && w_r < 8'd8)  : begin w_symbol = 5'd 0; w_ks = 3'd3; w_rem = w_r - 8'd0 ; w_escape = 1'd0; end
                        (w_r >= 8'd8  && w_r < 8'd16) : begin w_symbol = 5'd 1; w_ks = 3'd3; w_rem = w_r - 8'd8 ; w_escape = 1'd0; end
                        (w_r >= 8'd16 && w_r < 8'd20) : begin w_symbol = 5'd 2; w_ks = 3'd2; w_rem = w_r - 8'd16; w_escape = 1'd0; end
                        (w_r >= 8'd20 && w_r < 8'd24) : begin w_symbol = 5'd 3; w_ks = 3'd2; w_rem = w_r - 8'd20; w_escape = 1'd0; end
                        (w_r >= 8'd24 && w_r < 8'd28) : begin w_symbol = 5'd 4; w_ks = 3'd2; w_rem = w_r - 8'd24; w_escape = 1'd0; end
                        (w_r == 8'd28)                : begin w_symbol = 5'd 5; w_ks = 3'd0; w_rem = w_r - 8'd28; w_escape = 1'd0; end
                        (w_r == 8'd29)                : begin w_symbol = 5'd 6; w_ks = 3'd0; w_rem = w_r - 8'd29; w_escape = 1'd0; end
                        (w_r == 8'd30)                : begin w_symbol = 5'd 7; w_ks = 3'd0; w_rem = w_r - 8'd30; w_escape = 1'd0; end
                        (w_r == 8'd31)                : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = w_r - 8'd31; w_escape = 1'd1; end
                        default                       : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;        w_escape = 1'd0; end
                    endcase
                end

                2'd3: begin
                    case (1'b1)
                        (w_r >= 8'd0  && w_r < 8'd4)  : begin w_symbol = 5'd 0; w_ks = 3'd2; w_rem = w_r - 8'd0 ; w_escape = 1'd0; end
                        (w_r >= 8'd4  && w_r < 8'd8)  : begin w_symbol = 5'd 1; w_ks = 3'd2; w_rem = w_r - 8'd4 ; w_escape = 1'd0; end
                        (w_r >= 8'd8  && w_r < 8'd12) : begin w_symbol = 5'd 2; w_ks = 3'd2; w_rem = w_r - 8'd8 ; w_escape = 1'd0; end
                        (w_r >= 8'd12 && w_r < 8'd16) : begin w_symbol = 5'd 3; w_ks = 3'd2; w_rem = w_r - 8'd12; w_escape = 1'd0; end
                        (w_r >= 8'd16 && w_r < 8'd20) : begin w_symbol = 5'd 4; w_ks = 3'd2; w_rem = w_r - 8'd16; w_escape = 1'd0; end
                        (w_r >= 8'd20 && w_r < 8'd24) : begin w_symbol = 5'd 5; w_ks = 3'd2; w_rem = w_r - 8'd20; w_escape = 1'd0; end
                        (w_r >= 8'd24 && w_r < 8'd28) : begin w_symbol = 5'd 6; w_ks = 3'd2; w_rem = w_r - 8'd24; w_escape = 1'd0; end
                        (w_r >= 8'd28 && w_r < 8'd30) : begin w_symbol = 5'd 7; w_ks = 3'd1; w_rem = w_r - 8'd28; w_escape = 1'd0; end
                        (w_r >= 8'd30 && w_r < 8'd32) : begin w_symbol = 5'd 0; w_ks = 3'd1; w_rem = w_r - 8'd30; w_escape = 1'd1; end
                        default                       : begin w_symbol = 5'd 0; w_ks = 3'd0; w_rem = 8'd0;        w_escape = 1'd0; end
                    endcase
                end
            endcase
        end

        3'd6: begin
            w_symbol = {2'b0, w_r[2:0]};
            w_ks     = 3'd0;
            w_rem    = 8'd0;
            w_escape = 1'd0;
        end

        default: begin
            w_symbol = 5'd0;
            w_ks     = 3'd0;
            w_rem    = 8'd0;
            w_escape = 1'd0;
        end
    endcase
end