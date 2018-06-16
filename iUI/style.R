style <- tags$style(
  HTML("/* Smaller font for preformatted text */
       pre, table.table {
       font-size: smaller;
       }
       body {
       font-size: 12px;
       font-weight: bold;
       /*color: #ffffff;*/
       /*background: #000000;*/
       }
       .nm_fields {
       padding: 0px 2px;
       margin: 0px 0px;
       width: 90px;
       float: left;
       }
       .par_fields {
       padding: 0px 2px;
       margin: 0px 0px;
       width: 90px;
       float: left;
       }
       .hint_fields {
       padding: 0px 2px;
       margin: 0px 0px;
       width: 90px;
       float: left;
       }
       #ticker_search, #ticker_search_submit, #ticker_news, #ticker_news_submit,
       #eq_perf_period, #eq_perf_period_submit, #tb_perf_period, #tb_perf_period_submit, 
       #cb_perf_period, #cb_perf_period_submit, #add_trade_list, #add_trade_list_submit {
       padding: 0px 0px;
       margin: 0px 0px;
       height: 35px;
       }
       #portfolio_dt{
       /*background-color: #000000;*/
       }
       .add_trade_list, .add_trade_list_submit{
       width:200px;
       }
       "
  )
)