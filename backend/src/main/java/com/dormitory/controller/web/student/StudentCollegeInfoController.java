package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.qry.CollegeInfoQry;
import com.dormitory.controller.vo.CollegeInfoVO;
import com.dormitory.service.CollegeInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 学院信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/college/info")
@Api(value = "StudentCollegeInfoController", tags = {"学院信息接口"})
public class StudentCollegeInfoController {

    /**
     * 学院信息Service
     */
    private final CollegeInfoService collegeInfoService;

    @ApiOperation("学院信息列表查询")
    @GetMapping("/list")
    public R<List<CollegeInfoVO>> listByQry(CollegeInfoQry qry) {
        return R.success(collegeInfoService.listByQry(qry));
    }

    @ApiOperation("学院信息详情")
    @GetMapping("/detail/{collegeId}")
    public R<CollegeInfoVO> detail(@PathVariable @NotNull(message = "学院ID") Long collegeId) {
        return R.success(collegeInfoService.detailById(collegeId));
    }
}

