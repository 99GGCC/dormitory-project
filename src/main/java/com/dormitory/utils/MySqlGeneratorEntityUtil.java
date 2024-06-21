package com.dormitory.utils;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.generator.AutoGenerator;
import com.baomidou.mybatisplus.generator.config.DataSourceConfig;
import com.baomidou.mybatisplus.generator.config.GlobalConfig;
import com.baomidou.mybatisplus.generator.config.PackageConfig;
import com.baomidou.mybatisplus.generator.config.StrategyConfig;
import com.baomidou.mybatisplus.generator.config.po.TableFill;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;

import java.util.ArrayList;
import java.util.List;

/**
 * 代码生成器工具类
 *
 * @author XXX
 */
public class MySqlGeneratorEntityUtil {
    public static void main(String[] args) {
        AutoGenerator autoGenerator = new AutoGenerator();
        DataSourceConfig dataSourceConfig = new DataSourceConfig();
        dataSourceConfig.setDbType(DbType.MYSQL);
        dataSourceConfig.setDriverName("com.mysql.cj.jdbc.Driver");
        dataSourceConfig.setUsername("dormitory_management");
        dataSourceConfig.setPassword("dormitory_management");
        dataSourceConfig.setUrl("jdbc:mysql://175.178.33.84:3306/dormitory_management?useUnicode=true&characterEncoding=UTF-8");
        autoGenerator.setDataSource(dataSourceConfig);
        GlobalConfig globalConfig = new GlobalConfig();
        globalConfig.setOpen(true);
        globalConfig.setOutputDir(System.getProperty("user.dir") + "/src/main/java");
        globalConfig.setAuthor("XXX");
        globalConfig.setServiceName("%sService");
        autoGenerator.setGlobalConfig(globalConfig);
        PackageConfig packageConfig = new PackageConfig();
        packageConfig.setParent("com.dormitory");
        packageConfig.setEntity("entity");
        packageConfig.setMapper("mapper");
        packageConfig.setXml("mapper.mapper");
        packageConfig.setController("controller");
        packageConfig.setService("service");
        packageConfig.setServiceImpl("service.impl");
        autoGenerator.setPackageInfo(packageConfig);
        StrategyConfig strategyConfig = getStrategyConfig();

        autoGenerator.setStrategy(strategyConfig);

        autoGenerator.execute();


    }

    private static StrategyConfig getStrategyConfig() {
        StrategyConfig strategyConfig = new StrategyConfig();
        strategyConfig.setEntityLombokModel(true);
        strategyConfig.setNaming(NamingStrategy.underline_to_camel);
        //驼峰命名
        strategyConfig.setColumnNaming(NamingStrategy.underline_to_camel);

        //设置自动填充字段
        List<TableFill> list = new ArrayList<>();
        TableFill tableFill1 = new TableFill("create_id", FieldFill.INSERT);
        TableFill tableFill2 = new TableFill("create_name", FieldFill.INSERT);
        TableFill tableFill3 = new TableFill("create_time", FieldFill.INSERT);
        TableFill tableFill4 = new TableFill("update_id", FieldFill.INSERT_UPDATE);
        TableFill tableFill5 = new TableFill("update_name", FieldFill.INSERT_UPDATE);
        TableFill tableFill6 = new TableFill("update_time", FieldFill.INSERT_UPDATE);
        TableFill tableFill7 = new TableFill("delete_flag", FieldFill.INSERT);
        list.add(tableFill1);
        list.add(tableFill2);
        list.add(tableFill3);
        list.add(tableFill4);
        list.add(tableFill5);
        list.add(tableFill6);
        list.add(tableFill7);

        strategyConfig.setTableFillList(list);
        return strategyConfig;
    }
}
