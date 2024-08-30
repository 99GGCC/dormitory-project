package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.qry.DormitoryInfoQry;
import com.dormitory.controller.vo.DormitoryInfoVO;
import com.dormitory.service.DormitoryInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * <p>
 * 宿舍信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/dormitory/info")
@Api(value = "StudentDormitoryInfoController", tags = {"宿舍信息接口"})
public class StudentDormitoryInfoController {

    /**
     * 宿舍信息Service
     */
    private final DormitoryInfoService dormitoryInfoService;

    @ApiOperation("宿舍信息列表查询")
    @GetMapping("/list")
    public R<List<DormitoryInfoVO>> listByQry(DormitoryInfoQry qry) {
        return R.success(dormitoryInfoService.listByQry(qry));
    }

}

