package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.qry.BuildingInfoQry;
import com.dormitory.controller.vo.BuildingInfoVO;
import com.dormitory.service.BuildingInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * <p>
 * 楼栋信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/building/info")
@Api(value = "StudentBuildingInfoController", tags = {"楼栋信息接口"})
public class StudentBuildingInfoController {

    /**
     * 楼栋信息Service
     */
    private final BuildingInfoService buildingInfoService;

    @ApiOperation("楼栋信息列表查询")
    @GetMapping("/list")
    public R<List<BuildingInfoVO>> listByQry(BuildingInfoQry qry) {
        return R.success(buildingInfoService.listByQry(qry));
    }
}

